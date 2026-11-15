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
import {
  ApiBadRequestResponse,
  ApiBody,
  ApiConsumes,
  ApiNotFoundResponse,
  ApiOkResponse,
  ApiPayloadTooLargeResponse,
  ApiOperation,
  ApiParam,
  ApiProduces,
  ApiTags,
} from "@nestjs/swagger";
import { Response } from "express";
import { createReadStream } from "fs";
import { unlink } from "fs/promises";
import { PinoLogger } from "nestjs-pino";
import { finished } from "stream/promises";
import { CustomBotService, PublishRequest } from "./custom-bot.service";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";
import { AuthenticatedUser } from "@/auth/auth.types";
import { CurrentUser } from "@/auth/current-user.decorator";
import { ApiJwtOrApiKeyAuth } from "@/swagger/api-auth.decorators";
import { StorageService } from "./storage.service";
import { appError } from "@/common/app-error";
import { toHttpException } from "@/common/http-error";
import { CustomBotUploadInterceptor } from "./custom-bot-upload.interceptor";

// Described by hand because the body is an interface, not a DTO class: as an
// undecorated class, the global ValidationPipe (forbidNonWhitelisted) would
// reject every field.
const DEPLOY_BODY_SCHEMA = {
  type: "object",
  required: ["config", "filePath"],
  properties: {
    config: {
      type: "string",
      description:
        "Custom bot config (name, version, runtime, entrypoints, schema, ...) " +
        "as a JSON string; `name` must match the path",
    },
    filePath: {
      type: "string",
      description: "Storage path returned by the upload endpoint",
    },
  },
};

const NAME_PARAM = { name: "name", description: "Custom bot name" };
const VERSION_PARAM = { name: "version", description: "Semver version" };

@ApiTags("custom-bots")
@ApiJwtOrApiKeyAuth()
@Controller("custom-bots")
@UseGuards(AuthCombinedGuard)
export class CustomBotController {
  constructor(
    private readonly customBotService: CustomBotService,
    private readonly storageService: StorageService,
    private readonly logger: PinoLogger,
  ) {}

  @Post(":name/upload")
  @HttpCode(HttpStatus.OK)
  @UseInterceptors(CustomBotUploadInterceptor)
  @ApiOperation({
    summary: "Upload a custom bot's zip archive",
    description: "Returns the storage path to pass to the deploy endpoints.",
  })
  @ApiParam(NAME_PARAM)
  @ApiConsumes("multipart/form-data")
  @ApiBody({
    schema: {
      type: "object",
      required: ["file", "version"],
      properties: {
        file: { type: "string", format: "binary", description: "ZIP archive" },
        version: { type: "string", description: "Semver version" },
      },
    },
  })
  @ApiBadRequestResponse({
    description: "Missing file or version, or not a zip",
  })
  @ApiPayloadTooLargeResponse({
    description: "ZIP archive exceeds CUSTOM_BOT_UPLOAD_MAX_BYTES",
  })
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

    try {
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

      const content = createReadStream(file.path);
      const closed = finished(content, { cleanup: true }).catch(
        (): undefined => undefined,
      );
      try {
        const uploadResult = await this.storageService.uploadBotFile(
          content,
          file.size,
          body.version,
          userId,
          name,
        );

        if (!uploadResult.success) {
          throw toHttpException(
            appError(
              uploadResult.error.code,
              `Upload failed: ${uploadResult.error.message}`,
            ),
          );
        }

        return {
          success: true,
          filePath: uploadResult.data,
          message: "File uploaded successfully",
        };
      } finally {
        // MinIO can reject before consuming the stream. Close it before unlinking.
        content.destroy();
        await closed;
      }
    } finally {
      try {
        await unlink(file.path);
      } catch (error: unknown) {
        if (
          !(
            error instanceof Error &&
            "code" in error &&
            error.code === "ENOENT"
          )
        ) {
          this.logger.warn(
            { err: error, filePath: file.path },
            "Failed to remove temporary bot upload",
          );
        }
      }
    }
  }

  @Post(":name")
  @HttpCode(HttpStatus.CREATED)
  @ApiOperation({ summary: "Deploy the first version of a custom bot" })
  @ApiParam(NAME_PARAM)
  @ApiBody({ schema: DEPLOY_BODY_SCHEMA })
  @ApiBadRequestResponse({ description: "Invalid config or file path" })
  async createCustomBot(
    @Param("name") name: string,
    @Body() body: PublishRequest,
    @CurrentUser() user: AuthenticatedUser,
  ) {
    const result = await this.customBotService.publishFirstVersion(
      user.uid,
      name,
      body,
    );

    if (!result.success) {
      throw toHttpException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Custom bot created successfully",
    };
  }

  @Put(":name")
  @HttpCode(HttpStatus.OK)
  @ApiOperation({ summary: "Deploy a new version of a custom bot" })
  @ApiParam(NAME_PARAM)
  @ApiBody({ schema: DEPLOY_BODY_SCHEMA })
  @ApiBadRequestResponse({ description: "Invalid config or file path" })
  async updateCustomBot(
    @Param("name") name: string,
    @Body() body: PublishRequest,
    @CurrentUser() user: AuthenticatedUser,
  ) {
    const result = await this.customBotService.publishVersion(
      user.uid,
      name,
      body,
    );

    if (!result.success) {
      throw toHttpException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Custom bot updated successfully",
    };
  }

  @Get()
  @HttpCode(HttpStatus.OK)
  @ApiOperation({ summary: "List your custom bots" })
  async getUserCustomBots(@CurrentUser() user: AuthenticatedUser) {
    const userId = user.uid;

    const result = await this.customBotService.getUserCustomBots(userId);

    if (!result.success) {
      throw toHttpException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "User custom bots retrieved successfully",
    };
  }

  @Get(":name/versions")
  @HttpCode(HttpStatus.OK)
  @ApiOperation({
    summary: "List your versions of a custom bot with their instance counts",
  })
  @ApiParam(NAME_PARAM)
  @ApiNotFoundResponse({ description: "Custom bot not found" })
  async getVersionsWithInstances(
    @Param("name") name: string,
    @CurrentUser() user: AuthenticatedUser,
  ) {
    const result = await this.customBotService.getVersionsWithInstanceCounts(
      user.uid,
      name,
    );

    if (!result.success) {
      throw toHttpException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Bot versions with instance counts retrieved successfully",
    };
  }

  @Get(":name")
  @HttpCode(HttpStatus.OK)
  @ApiOperation({ summary: "List every version of a custom bot" })
  @ApiParam(NAME_PARAM)
  @ApiNotFoundResponse({ description: "Custom bot not found" })
  async getAllVersions(@Param("name") name: string) {
    const result = await this.customBotService.findVersions(name);

    if (!result.success) {
      throw toHttpException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Bot versions retrieved successfully",
    };
  }

  @Get("by-id/:id/frontend")
  @ApiOperation({ summary: "Download a custom bot version's frontend bundle" })
  @ApiParam({ name: "id", description: "Custom bot version ID" })
  @ApiProduces("application/javascript")
  @ApiOkResponse({ description: "JavaScript bundle" })
  @ApiNotFoundResponse({ description: "Bot or frontend bundle not found" })
  async getFrontendBundleById(@Param("id") id: string, @Res() res: Response) {
    // Get custom bot by ID (includes specific version info)
    const result = await this.customBotService.findVersionById(id);

    if (!result.success) {
      throw toHttpException(
        result.error.code === "not_found"
          ? appError("not_found", `Custom bot not found: ${id}`)
          : result.error,
      );
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

    if (!streamResult.success) {
      throw toHttpException(streamResult.error);
    }

    // Serve the JavaScript bundle as a stream to avoid memory issues with large bundles
    res.setHeader("Content-Type", "application/javascript");
    res.setHeader("Cache-Control", "public, max-age=31536000, immutable");
    streamResult.data.pipe(res);
  }

  @Delete(":name/:version")
  @HttpCode(HttpStatus.OK)
  @ApiOperation({ summary: "Delete one version of a custom bot" })
  @ApiParam(NAME_PARAM)
  @ApiParam(VERSION_PARAM)
  @ApiBadRequestResponse({ description: "Version could not be deleted" })
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
      throw toHttpException(result.error);
    }

    return {
      success: true,
      message: `Version ${version} of '${name}' deleted successfully`,
    };
  }

  @Delete(":name")
  @HttpCode(HttpStatus.OK)
  @ApiOperation({ summary: "Delete every version of a custom bot" })
  @ApiParam(NAME_PARAM)
  @ApiBadRequestResponse({ description: "Custom bot could not be deleted" })
  async deleteAllVersions(
    @Param("name") name: string,
    @CurrentUser() user: AuthenticatedUser,
  ) {
    const result = await this.customBotService.deleteAllVersions(
      user.uid,
      name,
    );

    if (!result.success) {
      throw toHttpException(result.error);
    }

    return {
      success: true,
      message: `All versions of '${name}' deleted successfully`,
    };
  }

  @Get(":name/:version")
  @HttpCode(HttpStatus.OK)
  @ApiOperation({ summary: "Get one version of a custom bot" })
  @ApiParam(NAME_PARAM)
  @ApiParam(VERSION_PARAM)
  @ApiNotFoundResponse({ description: "Custom bot version not found" })
  async getSpecificVersion(
    @Param("name") name: string,
    @Param("version") version: string,
  ) {
    const result = await this.customBotService.findVersion(name, version);

    if (!result.success) {
      throw toHttpException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Bot version retrieved successfully",
    };
  }
}
