import { Injectable, Inject } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { PinoLogger } from "nestjs-pino";
import { Result, Ok, Failure } from "@/common/result";
import { MINIO_CLIENT } from "@/minio";
import * as Minio from "minio";
import AdmZip from "adm-zip";

@Injectable()
export class StorageService {
  private bucketName: string;

  constructor(
    @Inject(MINIO_CLIENT) private readonly minioClient: Minio.Client,
    private readonly configService: ConfigService,
    private readonly logger: PinoLogger,
  ) {
    this.bucketName =
      this.configService.get<string>("CUSTOM_BOTS_BUCKET") || "custom-bots";

    // Ensure bucket exists on startup
    this.ensureBucket().catch((error) => {
      this.logger.error({ err: error }, "Failed to ensure MinIO bucket exists");
    });
  }

  async ensureBucket(): Promise<Result<void, string>> {
    try {
      const exists = await this.minioClient.bucketExists(this.bucketName);
      if (!exists) {
        await this.minioClient.makeBucket(this.bucketName);
        this.logger.info({ bucket: this.bucketName }, "Created MinIO bucket");
      }
      return Ok(undefined);
    } catch (error) {
      return Failure(`Failed to ensure bucket exists: ${error.message}`);
    }
  }

  async fileExists(filePath: string): Promise<Result<boolean, string>> {
    try {
      await this.minioClient.statObject(this.bucketName, filePath);
      return Ok(true);
    } catch (error) {
      // MinIO throws error if object doesn't exist
      if (
        error.code === "NoSuchKey" ||
        error.message?.includes("does not exist")
      ) {
        return Ok(false);
      }
      return Failure(`Error checking file existence: ${error.message}`);
    }
  }

  async getFileInfo(filePath: string): Promise<Result<any, string>> {
    try {
      const stat = await this.minioClient.statObject(this.bucketName, filePath);
      return Ok({
        size: stat.size,
        lastModified: stat.lastModified,
        etag: stat.etag,
      });
    } catch (error) {
      return Failure(`Error getting file info: ${error.message}`);
    }
  }

  async validateZipStructure(
    filePath: string,
    requiredFiles: string[],
  ): Promise<Result<boolean, string>> {
    try {
      // Download the ZIP file from MinIO
      const stream = await this.minioClient.getObject(
        this.bucketName,
        filePath,
      );
      const chunks: Buffer[] = [];

      // Read stream into buffer
      for await (const chunk of stream) {
        chunks.push(chunk);
      }
      const buffer = Buffer.concat(chunks);

      // Parse ZIP and check for required files
      const zip = new AdmZip(buffer);
      const entries = zip.getEntries();

      // Check for required files
      for (const requiredFile of requiredFiles) {
        const found = entries.some((entry) => entry.entryName === requiredFile);
        if (!found) {
          return Failure(`Required file missing: ${requiredFile}`);
        }
      }

      return Ok(true);
    } catch (error) {
      return Failure(`Error validating ZIP structure: ${error.message}`);
    }
  }

  async uploadBotFile(
    content: Buffer,
    fileName: string,
    userId: string,
    botName: string,
  ): Promise<Result<string, string>> {
    try {
      const filePath = `${userId}/${botName}/${fileName}`;

      // Upload to MinIO with metadata
      await this.minioClient.putObject(
        this.bucketName,
        filePath,
        content,
        content.length,
        {
          "Content-Type": "application/zip",
          "X-Upload-Source": "custom-bot-api",
          "X-Upload-Time": new Date().toISOString(),
        },
      );

      this.logger.info({ filePath }, "Uploaded bot file to MinIO");
      return Ok(filePath);
    } catch (error) {
      return Failure(`Error uploading file: ${error.message}`);
    }
  }

  async downloadFile(filePath: string): Promise<Result<Buffer, string>> {
    try {
      const stream = await this.minioClient.getObject(
        this.bucketName,
        filePath,
      );
      const chunks: Buffer[] = [];

      // Read stream into buffer
      for await (const chunk of stream) {
        chunks.push(chunk);
      }

      const buffer = Buffer.concat(chunks);
      return Ok(buffer);
    } catch (error) {
      return Failure(`Error downloading file: ${error.message}`);
    }
  }

  async deleteFile(filePath: string): Promise<Result<void, string>> {
    try {
      await this.minioClient.removeObject(this.bucketName, filePath);
      return Ok(undefined);
    } catch (error) {
      return Failure(`Error deleting file: ${error.message}`);
    }
  }

  // Helper method to get object metadata
  async getObjectMetadata(filePath: string): Promise<Result<any, string>> {
    try {
      const stat = await this.minioClient.statObject(this.bucketName, filePath);
      return Ok({
        size: stat.size,
        lastModified: stat.lastModified,
        etag: stat.etag,
        metadata: stat.metaData,
      });
    } catch (error) {
      return Failure(`Error getting object metadata: ${error.message}`);
    }
  }

  // Helper method to list objects with prefix
  async listObjects(prefix: string): Promise<Result<string[], string>> {
    try {
      const objects: string[] = [];
      const stream = this.minioClient.listObjects(
        this.bucketName,
        prefix,
        true,
      );

      for await (const obj of stream) {
        objects.push(obj.name);
      }

      return Ok(objects);
    } catch (error) {
      return Failure(`Error listing objects: ${error.message}`);
    }
  }

  /**
   * Get bot frontend bundle from storage as a stream.
   * Uses streaming to avoid loading large bundles into memory.
   */
  async getBotFrontendStream(
    frontendPath: string,
  ): Promise<Result<NodeJS.ReadableStream, string>> {
    try {
      const stream = await this.minioClient.getObject(
        this.bucketName,
        frontendPath,
      );
      return Ok(stream);
    } catch (error) {
      if (
        error.code === "NoSuchKey" ||
        error.message?.includes("does not exist")
      ) {
        return Failure("Frontend bundle not found");
      }
      return Failure(`Error retrieving frontend bundle: ${error.message}`);
    }
  }

  /**
   * Extract frontend bundle from bot ZIP and store separately.
   * Looks for frontend/dist/bundle.js or frontend.js in the ZIP.
   * Returns the path where frontend was stored, or null if no frontend found.
   */
  async extractAndStoreFrontend(
    zipFilePath: string,
    userId: string,
    botName: string,
    version: string,
  ): Promise<Result<string | null, string>> {
    try {
      // Download the ZIP file
      const stream = await this.minioClient.getObject(
        this.bucketName,
        zipFilePath,
      );
      const chunks: Buffer[] = [];

      for await (const chunk of stream) {
        chunks.push(chunk);
      }
      const buffer = Buffer.concat(chunks);

      // Parse ZIP and look for frontend bundle
      const zip = new AdmZip(buffer);
      const entries = zip.getEntries();

      // Look for frontend bundle in common locations
      const frontendPaths = [
        "frontend/dist/bundle.js",
        "frontend/dist/index.js",
        "frontend/bundle.js",
        "frontend.js",
        "dist/frontend.js",
      ];

      let frontendEntry = null;
      for (const path of frontendPaths) {
        frontendEntry = entries.find((entry) => entry.entryName === path);
        if (frontendEntry) break;
      }

      if (!frontendEntry) {
        // No frontend found - this is not an error
        return Ok(null);
      }

      // Extract the frontend bundle content
      const frontendContent = frontendEntry.getData();

      // Store separately at a predictable path
      const frontendStoragePath = `${userId}/${botName}/${version}/frontend.js`;

      await this.minioClient.putObject(
        this.bucketName,
        frontendStoragePath,
        frontendContent,
        frontendContent.length,
        {
          "Content-Type": "application/javascript",
          "X-Extracted-From": zipFilePath,
          "X-Extract-Time": new Date().toISOString(),
        },
      );

      this.logger.info(
        { frontendStoragePath, sourceZip: zipFilePath },
        "Extracted and stored frontend bundle",
      );

      return Ok(frontendStoragePath);
    } catch (error) {
      return Failure(`Error extracting frontend bundle: ${error.message}`);
    }
  }

  /**
   * Get the frontend storage path for a bot version.
   */
  getFrontendPath(userId: string, botName: string, version: string): string {
    return `${userId}/${botName}/${version}/frontend.js`;
  }
}
