import { Injectable } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { Result, Ok, Failure } from "@/common/result";
import * as Minio from "minio";
import AdmZip from "adm-zip";

@Injectable()
export class StorageService {
  private minioClient: Minio.Client;
  private bucketName: string;

  constructor(private readonly configService: ConfigService) {
    const endpoint =
      this.configService.get<string>("MINIO_ENDPOINT") || "localhost";
    const port = parseInt(
      this.configService.get<string>("MINIO_PORT") || "9000",
    );

    // Internal client for API operations
    this.minioClient = new Minio.Client({
      endPoint: endpoint,
      port,
      useSSL: this.configService.get<string>("MINIO_USE_SSL") === "true",
      accessKey:
        this.configService.get<string>("MINIO_ACCESS_KEY") || "minioadmin",
      secretKey:
        this.configService.get<string>("MINIO_SECRET_KEY") || "minioadmin",
    });

    this.bucketName =
      this.configService.get<string>("CUSTOM_BOTS_BUCKET") || "custom-bots";

    // Ensure bucket exists on startup
    this.ensureBucket().catch((error) => {
      console.error("Failed to ensure MinIO bucket exists:", error);
    });
  }

  async ensureBucket(): Promise<Result<void, string>> {
    try {
      const exists = await this.minioClient.bucketExists(this.bucketName);
      if (!exists) {
        await this.minioClient.makeBucket(this.bucketName);
        console.log(`✅ Created MinIO bucket: ${this.bucketName}`);
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

      console.log(`✅ Uploaded bot file to MinIO: ${filePath}`);
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
}
