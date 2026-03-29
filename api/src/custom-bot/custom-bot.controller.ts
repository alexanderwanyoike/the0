import {
  Controller,
  Post,
  Put,
  Get,
  Delete,
  Body,
  Param,
  BadRequestException,
  NotFoundException,
  Res,
  HttpStatus,
  HttpCode,
  UseGuards,
  UseInterceptors,
  UploadedFile,
} from "@nestjs/common";
import { FileInterceptor } from "@nestjs/platform-express";
import { Response } from "express";
import { CustomBotService } from "./custom-bot.service";
import { CustomBotConfig } from "./custom-bot.types";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";
import { AuthenticatedUser } from "@/auth/auth.types";
import { CurrentUser } from "@/auth/current-user.decorator";
import { StorageService } from "./storage.service";

interface CustomBotDeployDto {
  config: string; // JSON string
  filePath: string; // Storage path where file was uploaded
}

@Controller("custom-bots")
@UseGuards(AuthCombinedGuard)
export class CustomBotController {
  constructor(
    private readonly customBotService: CustomBotService,
    private readonly storageService: StorageService,
  ) {}

  private validateFilePath(filePath: unknown, userId: string): string {
    if (typeof filePath !== "string") {
      throw new BadRequestException("filePath must be a string");
    }
    if (filePath.trim() === "") {
      throw new BadRequestException("filePath is required");
    }
    const trimmed = filePath.trim();
    if (trimmed.includes("..") || trimmed.includes("//")) {
      throw new BadRequestException("Invalid file path");
    }
    if (!trimmed.startsWith(`${userId}/`)) {
      throw new BadRequestException(
        "File path must belong to the authenticated user",
      );
    }
    return trimmed;
  }

  @Post(":name/upload")
  @HttpCode(HttpStatus.OK)
  @UseInterceptors(FileInterceptor("file"))
  async uploadFile(
    @Param("name") name: string,
    @UploadedFile() file: Express.Multer.File,
    @Body() body: { version?: string },
    @CurrentUser() user: AuthenticatedUser,
  ) {
    const userId = user.uid;

    if (!file) {
      throw new BadRequestException("File is required");
    }

    // Validate file type
    if (
      file.mimetype !== "application/zip" &&
      !file.originalname.endsWith(".zip")
    ) {
      throw new BadRequestException("Only ZIP files are allowed");
    }

    // Version must be provided
    if (!body.version) {
      throw new BadRequestException("Version is required");
    }

    // Upload to MinIO using internal client
    const uploadResult = await this.storageService.uploadBotFile(
      file.buffer,
      body.version,
      userId,
      name,
    );

    if (!uploadResult.success) {
      throw new BadRequestException(`Upload failed: ${uploadResult.error}`);
    }

    return {
      success: true,
      filePath: uploadResult.data,
      message: "File uploaded successfully",
    };
  }

  @Post(":name")
  @HttpCode(HttpStatus.CREATED)
  async createCustomBot(
    @Param("name") name: string,
    @Body() body: CustomBotDeployDto,
    @CurrentUser() user: AuthenticatedUser,
  ) {
    const userId = user.uid;
    const filePath = this.validateFilePath(body.filePath, userId);

    const fileExistsResult = await this.storageService.fileExists(filePath);
    if (!fileExistsResult.success) {
      throw new BadRequestException(
        `File validation failed: ${fileExistsResult.error}`,
      );
    }

    if (!fileExistsResult.data) {
      throw new BadRequestException("File not found at specified file path");
    }

    if (!body.config) {
      throw new BadRequestException("Config field is required");
    }

    let config: CustomBotConfig;
    try {
      config = JSON.parse(body.config);
    } catch (error) {
      throw new BadRequestException("Config must be valid JSON");
    }

    if (config.name !== name) {
      throw new BadRequestException(
        "Bot name in config must match URL parameter",
      );
    }

    const result = await this.customBotService.createCustomBot(
      userId,
      config,
      filePath,
    );

    if (!result.success) {
      throw new BadRequestException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Custom bot created successfully",
    };
  }

  @Put(":name")
  @HttpCode(HttpStatus.OK)
  async updateCustomBot(
    @Param("name") name: string,
    @Body() body: CustomBotDeployDto,
    @CurrentUser() user: AuthenticatedUser,
  ) {
    const userId = user.uid;
    const filePath = this.validateFilePath(body.filePath, userId);

    const fileExistsResult = await this.storageService.fileExists(filePath);
    if (!fileExistsResult.success) {
      throw new BadRequestException(
        `File validation failed: ${fileExistsResult.error}`,
      );
    }

    if (!fileExistsResult.data) {
      throw new BadRequestException("File not found at specified file path");
    }

    if (!body.config) {
      throw new BadRequestException("Config field is required");
    }

    let config: CustomBotConfig;
    try {
      config = JSON.parse(body.config);
    } catch (error) {
      throw new BadRequestException("Config must be valid JSON");
    }

    if (config.name !== name) {
      throw new BadRequestException(
        "Bot name in config must match URL parameter",
      );
    }

    const result = await this.customBotService.updateCustomBot(
      userId,
      name,
      config,
      filePath,
    );

    if (!result.success) {
      throw new BadRequestException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Custom bot updated successfully",
    };
  }

  @Get()
  @HttpCode(HttpStatus.OK)
  async getUserCustomBots(@CurrentUser() user: AuthenticatedUser) {
    const userId = user.uid;

    const result = await this.customBotService.getUserCustomBots(userId);

    if (!result.success) {
      throw new BadRequestException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "User custom bots retrieved successfully",
    };
  }

  @Get(":name/versions")
  @HttpCode(HttpStatus.OK)
  async getVersionsWithInstances(
    @Param("name") name: string,
    @CurrentUser() user: AuthenticatedUser,
  ) {
    const result = await this.customBotService.getVersionsWithInstanceCounts(
      user.uid,
      name,
    );

    if (!result.success) {
      throw new NotFoundException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Bot versions with instance counts retrieved successfully",
    };
  }

  @Get(":name")
  @HttpCode(HttpStatus.OK)
  async getAllVersions(@Param("name") name: string) {
    const result = await this.customBotService.getAllGlobalVersions(name);

    if (!result.success) {
      throw new NotFoundException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Bot versions retrieved successfully",
    };
  }

  @Get("by-id/:id/frontend")
  async getFrontendBundleById(@Param("id") id: string, @Res() res: Response) {
    // Get custom bot by ID (includes specific version info)
    const result = await this.customBotService.getById(id);

    if (!result.success || !result.data) {
      throw new NotFoundException(`Custom bot not found: ${id}`);
    }

    const customBot = result.data;

    // Check if bot has frontend
    if (!customBot.config.hasFrontend) {
      throw new NotFoundException("This bot does not have a custom frontend");
    }

    // Get frontend bundle from storage as a stream
    // Frontend is stored separately at: {userId}/{botName}/{version}/frontend.js
    const frontendPath = this.storageService.getFrontendPath(
      customBot.userId,
      customBot.name,
      customBot.version,
    );
    const streamResult =
      await this.storageService.getBotFrontendStream(frontendPath);

    if (!streamResult.success || !streamResult.data) {
      throw new NotFoundException("Frontend bundle not found");
    }

    // Serve the JavaScript bundle as a stream to avoid memory issues with large bundles
    res.setHeader("Content-Type", "application/javascript");
    res.setHeader("Cache-Control", "public, max-age=31536000, immutable");
    streamResult.data.pipe(res);
  }

  @Delete(":name/:version")
  @HttpCode(HttpStatus.OK)
  async deleteVersion(
    @Param("name") name: string,
    @Param("version") version: string,
    @CurrentUser() user: AuthenticatedUser,
  ) {
    const result = await this.customBotService.deleteVersion(
      user.uid,
      name,
      version,
    );

    if (!result.success) {
      throw new BadRequestException(result.error);
    }

    return {
      success: true,
      message: `Version ${version} of '${name}' deleted successfully`,
    };
  }

  @Delete(":name")
  @HttpCode(HttpStatus.OK)
  async deleteAllVersions(
    @Param("name") name: string,
    @CurrentUser() user: AuthenticatedUser,
  ) {
    const result = await this.customBotService.deleteAllVersions(
      user.uid,
      name,
    );

    if (!result.success) {
      throw new BadRequestException(result.error);
    }

    return {
      success: true,
      message: `All versions of '${name}' deleted successfully`,
    };
  }

  @Get(":name/:version")
  @HttpCode(HttpStatus.OK)
  async getSpecificVersion(
    @Param("name") name: string,
    @Param("version") version: string,
  ) {
    const result = await this.customBotService.getGlobalSpecificVersion(
      name,
      version,
    );

    if (!result.success) {
      throw new NotFoundException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Bot version retrieved successfully",
    };
  }
}
