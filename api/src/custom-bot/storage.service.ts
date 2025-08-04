import { Injectable } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { Result, Ok, Failure } from '@/common/result';
import * as crypto from 'crypto';
import * as Minio from 'minio';
import AdmZip from 'adm-zip';
import { Readable } from 'stream';

// Simple in-memory token storage (in production, use Redis)
const uploadTokens = new Map<string, string>();

@Injectable()
export class StorageService {
  private minioClient: Minio.Client;
  private externalMinioClient: Minio.Client;
  private bucketName: string;

  constructor(private readonly configService: ConfigService) {
    const endpoint = this.configService.get<string>('MINIO_ENDPOINT') || 'localhost';
    const externalEndpoint = this.configService.get<string>('MINIO_EXTERNAL_ENDPOINT') || endpoint;
    const port = parseInt(this.configService.get<string>('MINIO_PORT') || '9000');
    
    // Internal client for API operations
    this.minioClient = new Minio.Client({
      endPoint: endpoint,
      port,
      useSSL: this.configService.get<string>('MINIO_USE_SSL') === 'true',
      accessKey: this.configService.get<string>('MINIO_ACCESS_KEY') || 'minioadmin',
      secretKey: this.configService.get<string>('MINIO_SECRET_KEY') || 'minioadmin',
    });

    // External client for generating signed URLs accessible from outside
    const [externalHost, externalPortStr] = externalEndpoint.split(':');
    const externalPort = externalPortStr ? parseInt(externalPortStr) : port;
    
    this.externalMinioClient = new Minio.Client({
      endPoint: externalHost,
      port: externalPort,
      useSSL: this.configService.get<string>('MINIO_USE_SSL') === 'true',
      accessKey: this.configService.get<string>('MINIO_ACCESS_KEY') || 'minioadmin',
      secretKey: this.configService.get<string>('MINIO_SECRET_KEY') || 'minioadmin',
    });

    this.bucketName = this.configService.get<string>('CUSTOM_BOTS_BUCKET') || 'custom-bots';
    
    // Ensure bucket exists on startup
    this.ensureBucket().catch(error => {
      console.error('Failed to ensure MinIO bucket exists:', error);
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
      if (error.code === 'NoSuchKey' || error.message?.includes('does not exist')) {
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
    requiredFiles: string[]
  ): Promise<Result<boolean, string>> {
    try {
      // Download the ZIP file from MinIO
      const stream = await this.minioClient.getObject(this.bucketName, filePath);
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
        const found = entries.some(entry => entry.entryName === requiredFile);
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
    botName: string
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
          'Content-Type': 'application/zip',
          'X-Upload-Source': 'custom-bot-api',
          'X-Upload-Time': new Date().toISOString(),
        }
      );

      console.log(`✅ Uploaded bot file to MinIO: ${filePath}`);
      return Ok(filePath);
    } catch (error) {
      return Failure(`Error uploading file: ${error.message}`);
    }
  }

  async downloadFile(filePath: string): Promise<Result<Buffer, string>> {
    try {
      const stream = await this.minioClient.getObject(this.bucketName, filePath);
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

  async generateSignedUploadUrl(
    userId: string,
    name: string,
    version: string
  ): Promise<Result<{
    uploadUrl: string;
    filePath: string;
    expiresAt: string;
  }, string>> {
    try {
      const filePath = `${userId}/${name}/${version}`;
      
      // Generate presigned URL (24 hours expiry) using external client for external access
      const expiry = 24 * 60 * 60; // 24 hours in seconds
      const uploadUrl = await this.externalMinioClient.presignedPutObject(
        this.bucketName,
        filePath,
        expiry
      );

      const expiresAt = new Date(Date.now() + expiry * 1000).toISOString();
      
      console.log(`📡 Generated signed upload URL: ${uploadUrl}`);
      
      return Ok({
        uploadUrl,
        filePath,
        expiresAt,
      });
    } catch (error) {
      console.error('❌ Error generating upload URL:', error);
      return Failure(`Error generating upload URL: ${error.message || error}`);
    }
  }

  // Token-based upload methods (kept for compatibility)
  async generateUploadToken(
    userId: string,
    botName: string,
    version: string
  ): Promise<Result<string, string>> {
    try {
      const token = crypto.randomBytes(32).toString('hex');
      const key = `${userId}:${botName}:${version}`;
      
      uploadTokens.set(token, key);
      
      // Clean up token after 1 hour
      setTimeout(() => {
        uploadTokens.delete(token);
      }, 60 * 60 * 1000);
      
      return Ok(token);
    } catch (error) {
      return Failure(`Error generating upload token: ${error.message}`);
    }
  }

  async validateUploadToken(token: string): Promise<Result<{
    userId: string;
    botName: string;
    version: string;
  }, string>> {
    try {
      const key = uploadTokens.get(token);
      if (!key) {
        return Failure('Invalid or expired upload token');
      }
      
      const [userId, botName, version] = key.split(':');
      return Ok({ userId, botName, version });
    } catch (error) {
      return Failure(`Error validating upload token: ${error.message}`);
    }
  }

  async uploadWithToken(
    token: string,
    content: Buffer
  ): Promise<Result<string, string>> {
    try {
      const tokenResult = await this.validateUploadToken(token);
      if (!tokenResult.success) {
        return Failure(tokenResult.error);
      }
      
      const { userId, botName, version } = tokenResult.data;
      
      const uploadResult = await this.uploadBotFile(content, version, userId, botName);
      if (!uploadResult.success) {
        return Failure(uploadResult.error);
      }
      
      // Clean up token after successful upload
      uploadTokens.delete(token);
      
      return Ok(uploadResult.data);
    } catch (error) {
      return Failure(`Error uploading with token: ${error.message}`);
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
      const stream = this.minioClient.listObjects(this.bucketName, prefix, true);
      
      for await (const obj of stream) {
        objects.push(obj.name);
      }
      
      return Ok(objects);
    } catch (error) {
      return Failure(`Error listing objects: ${error.message}`);
    }
  }
}