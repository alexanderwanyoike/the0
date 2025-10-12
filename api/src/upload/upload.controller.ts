import {
  Controller,
  Put,
  Param,
  HttpException,
  HttpStatus,
  Req,
} from "@nestjs/common";
import { Request } from "express";
import { StorageService } from "../custom-bot/storage.service";

@Controller("api/upload")
export class UploadController {
  constructor(private readonly storageService: StorageService) {}

  @Put(":token")
  async uploadFile(@Param("token") token: string, @Req() req: Request) {
    // Validate the upload token
    if (!token || token.length !== 64) {
      throw new HttpException("Invalid upload token", HttpStatus.UNAUTHORIZED);
    }

    // Check content type
    if (req.headers["content-type"] !== "application/zip") {
      throw new HttpException(
        "Only zip files are allowed",
        HttpStatus.BAD_REQUEST,
      );
    }

    // Validate upload token with storage service
    const tokenResult = await this.storageService.validateUploadToken(token);
    if (!tokenResult.success) {
      throw new HttpException(
        "Invalid or expired upload token",
        HttpStatus.UNAUTHORIZED,
      );
    }

    try {
      // Read the request body into a buffer
      const chunks: Buffer[] = [];

      for await (const chunk of req) {
        chunks.push(chunk);
      }

      const fileBuffer = Buffer.concat(chunks);

      // Upload to MinIO using the token
      const uploadResult = await this.storageService.uploadWithToken(
        token,
        fileBuffer,
      );

      if (!uploadResult.success) {
        throw new HttpException(
          `Upload failed: ${uploadResult.error}`,
          HttpStatus.INTERNAL_SERVER_ERROR,
        );
      }

      return {
        success: true,
        message: "File uploaded successfully",
        file: {
          filename: uploadResult.data.split("/").pop() || "bot.zip",
          originalName: "bot.zip",
          size: fileBuffer.length,
          path: uploadResult.data,
        },
      };
    } catch (error) {
      throw new HttpException(
        `Upload failed: ${error.message}`,
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }
}
