import {
  Controller,
  Post,
  Put,
  Get,
  Body,
  Param,
  BadRequestException,
  NotFoundException,
  Req,
  HttpStatus,
  HttpCode,
  UseGuards,
  Patch,
  UseInterceptors,
  UploadedFile,
} from "@nestjs/common";
import { FileInterceptor } from "@nestjs/platform-express";
import { Request } from "express";
import { CustomBotService } from "./custom-bot.service";
import { CustomBotConfig } from "./custom-bot.types";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";
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

  @Post(":name/upload")
  @HttpCode(HttpStatus.OK)
  @UseInterceptors(FileInterceptor("file"))
  async uploadFile(
    @Param("name") name: string,
    @UploadedFile() file: Express.Multer.File,
    @Body() body: { version?: string },
    @Req() request: Request,
  ) {
    const userId = (request as any).user?.uid;
    if (!userId) {
      throw new BadRequestException("User ID is required");
    }

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
    @Req() request: Request,
  ) {
    const userId = (request as any).user?.uid;
    if (!userId) {
      throw new BadRequestException("User ID is required");
    }

    // Validate file path
    if (!body.filePath) {
      throw new BadRequestException("file path is required");
    }

    // Validate that file exists at file path
    const fileExistsResult = await this.storageService.fileExists(
      body.filePath,
    );
    if (!fileExistsResult.success) {
      throw new BadRequestException(
        `File validation failed: ${fileExistsResult.error}`,
      );
    }

    if (!fileExistsResult.data) {
      throw new BadRequestException("File not found at specified file path");
    }

    // Validate and parse config
    if (!body.config) {
      throw new BadRequestException("Config field is required");
    }

    let config: CustomBotConfig;
    try {
      config = JSON.parse(body.config);
    } catch (error) {
      throw new BadRequestException("Config must be valid JSON");
    }

    // Ensure the name in config matches URL parameter
    if (config.name !== name) {
      throw new BadRequestException(
        "Bot name in config must match URL parameter",
      );
    }

    const result = await this.customBotService.createCustomBot(
      userId,
      config,
      body.filePath,
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
    @Req() request: Request,
  ) {
    const userId = (request as any).user?.uid;
    if (!userId) {
      throw new BadRequestException("User ID is required");
    }

    // Validate file path
    if (!body.filePath) {
      throw new BadRequestException("file path is required");
    }

    // Validate that file exists at file path
    const fileExistsResult = await this.storageService.fileExists(
      body.filePath,
    );
    if (!fileExistsResult.success) {
      throw new BadRequestException(
        `File validation failed: ${fileExistsResult.error}`,
      );
    }

    if (!fileExistsResult.data) {
      throw new BadRequestException("File not found at specified file path");
    }

    // Validate and parse config
    if (!body.config) {
      throw new BadRequestException("Config field is required");
    }

    let config: CustomBotConfig;
    try {
      config = JSON.parse(body.config);
    } catch (error) {
      throw new BadRequestException("Config must be valid JSON");
    }

    // Ensure the name in config matches URL parameter
    if (config.name !== name) {
      throw new BadRequestException(
        "Bot name in config must match URL parameter",
      );
    }

    const result = await this.customBotService.updateCustomBot(
      userId,
      name,
      config,
      body.filePath,
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
  async getUserCustomBots(@Req() request: Request) {
    const userId = (request as any).user?.uid;
    if (!userId) {
      throw new BadRequestException("User ID is required");
    }

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

  @Get(":name")
  @HttpCode(HttpStatus.OK)
  async getAllVersions(@Param("name") name: string, @Req() request: Request) {
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

  @Get(":name/:version")
  @HttpCode(HttpStatus.OK)
  async getSpecificVersion(
    @Param("name") name: string,
    @Param("version") version: string,
    @Req() request: Request,
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
