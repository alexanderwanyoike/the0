import { Injectable } from "@nestjs/common";
import { PinoLogger } from "nestjs-pino";
import * as semver from "semver";
import { CustomBotRepository } from "./custom-bot.repository";
import { StorageService } from "@/custom-bot/storage.service";
import { validateCustomBotConfigPayload } from "./custom-bot.schema";

import {
  CustomBotConfig,
  CustomBot,
  CustomBotWithVersions,
  VersionWithInstances,
  SUPPORTED_RUNTIMES,
  COMPILED_RUNTIMES,
} from "./custom-bot.types";
import { appError, AppError } from "@/common/app-error";
import { Result, Failure, Ok, errorMessage } from "@/common/result";
import { WRITE_FAILED } from "@/common/role.repository";

/** A publish request as a client sends it: the config is JSON text. */
export interface PublishRequest {
  config: unknown;
  filePath: unknown;
}

@Injectable()
export class CustomBotService {
  constructor(
    private readonly customBotRepository: CustomBotRepository,
    private readonly storageService: StorageService,
    private readonly logger: PinoLogger,
  ) {}

  /** Publishes the first version of a new Custom bot, as its Uploader. */
  async publishFirstVersion(
    userId: string,
    name: string,
    request: PublishRequest,
  ): Promise<Result<CustomBot, AppError>> {
    const parsed = await this.parsePublishRequest(userId, name, request);
    if (!parsed.success) return Failure(parsed.error);
    const { config, filePath } = parsed.data;
    try {
      // Validate config structure
      const validation = validateCustomBotConfigPayload(config);
      if (!validation.valid) {
        return Failure(
          appError(
            "validation",
            `Validation failed: ${validation.errors?.join(", ")}`,
          ),
        );
      }

      // Check if bot already exists
      const existsResult = await this.customBotRepository.globalBotExists(
        config.name,
      );
      if (!existsResult.success) {
        return Failure(existsResult.error);
      }

      if (!config.runtime || !SUPPORTED_RUNTIMES.includes(config.runtime)) {
        return Failure(
          appError(
            "validation",
            `Bots must specify a valid runtime (${SUPPORTED_RUNTIMES.join(", ")})`,
          ),
        );
      }

      if (existsResult.data) {
        return Failure(
          appError("validation", "Custom bot with this name already exists"),
        );
      }

      return await this.storeVersion(
        userId,
        config,
        filePath,
        "Frontend bundle extracted for bot",
      );
    } catch (error: unknown) {
      this.logger.error({ err: error }, "Error creating custom bot");
      return Failure(
        appError(
          "internal",
          `Failed to create custom bot: ${errorMessage(error)}`,
        ),
      );
    }
  }

  /**
   * The checks a publish request passes before its config is read as a
   * Custom bot: the upload must be the caller's own and exist, and the
   * config must be JSON naming the Custom bot being published.
   */
  private async parsePublishRequest(
    userId: string,
    name: string,
    request: PublishRequest,
  ): Promise<Result<{ config: CustomBotConfig; filePath: string }, AppError>> {
    const filePath = this.checkFilePath(request.filePath, userId);
    if (!filePath.success) return Failure(filePath.error);

    const fileExists = await this.storageService.fileExists(filePath.data);
    if (!fileExists.success) {
      return Failure(
        appError(
          fileExists.error.code,
          `File validation failed: ${fileExists.error.message}`,
        ),
      );
    }
    if (!fileExists.data) {
      return invalid("File not found at specified file path");
    }

    if (!request.config) {
      return invalid("Config field is required");
    }

    let config: CustomBotConfig;
    try {
      config = JSON.parse(request.config as string);
    } catch {
      return invalid("Config must be valid JSON");
    }

    if (config.name !== name) {
      return invalid("Bot name in config must match URL parameter");
    }

    return Ok({ config, filePath: filePath.data });
  }

  private checkFilePath(
    filePath: unknown,
    userId: string,
  ): Result<string, AppError> {
    if (typeof filePath !== "string") {
      return invalid("filePath must be a string");
    }
    const trimmed = filePath.trim();
    if (trimmed === "") {
      return invalid("filePath is required");
    }
    if (trimmed.includes("..") || trimmed.includes("//")) {
      return invalid("Invalid file path");
    }
    if (!trimmed.startsWith(`${userId}/`)) {
      return invalid("File path must belong to the authenticated user");
    }
    return Ok(trimmed);
  }

  private async storeVersion(
    userId: string,
    config: CustomBotConfig,
    filePath: string,
    frontendLogMessage: string,
  ): Promise<Result<CustomBot, AppError>> {
    const requiredFiles = COMPILED_RUNTIMES.includes(config.runtime)
      ? []
      : Object.values(config.entrypoints).filter(Boolean);
    const zipValidation = await this.storageService.validateZipStructure(
      filePath,
      requiredFiles,
    );
    if (!zipValidation.success) {
      return Failure(
        appError(
          zipValidation.error.code,
          `ZIP validation failed: ${zipValidation.error.message}`,
        ),
      );
    }

    const frontendResult = await this.storageService.extractAndStoreFrontend(
      filePath,
      userId,
      config.name,
      config.version,
    );
    const finalConfig = {
      ...config,
      hasFrontend: frontendResult.success && frontendResult.data !== null,
    };
    if (frontendResult.success && frontendResult.data) {
      this.logger.info(
        { botName: config.name, frontendPath: frontendResult.data },
        frontendLogMessage,
      );
    }

    const result = await this.customBotRepository.create({
      userId,
      name: finalConfig.name,
      version: finalConfig.version,
      config: finalConfig,
      filePath,
      status: "active",
    });
    if (!result.success) {
      return Failure(result.error);
    }

    return this.customBotRepository.findGlobalByKeyAndVersion(
      config.name,
      config.version,
    );
  }

  /** Publishes a new version of a Custom bot; only its Uploader may. */
  async publishVersion(
    userId: string,
    name: string,
    request: PublishRequest,
  ): Promise<Result<CustomBot, AppError>> {
    const parsed = await this.parsePublishRequest(userId, name, request);
    if (!parsed.success) return Failure(parsed.error);
    const { config, filePath } = parsed.data;
    try {
      const configCheck = this.checkUpdateConfig(config);
      if (!configCheck.success) return Failure(configCheck.error);

      const ownerCheck = await this.checkUserOwnsExistingBot(userId, name);
      if (!ownerCheck.success) return Failure(ownerCheck.error);

      const versionCheck = await this.checkVersionIsNew(name, config.version);
      if (!versionCheck.success) return Failure(versionCheck.error);

      return await this.storeVersion(
        userId,
        config,
        filePath,
        "Frontend bundle extracted for bot update",
      );
    } catch (error: unknown) {
      this.logger.error({ err: error }, "Error updating custom bot");
      return Failure(
        appError(
          "internal",
          `Failed to update custom bot: ${errorMessage(error)}`,
        ),
      );
    }
  }

  private checkUpdateConfig(config: CustomBotConfig): Result<null, AppError> {
    const validation = validateCustomBotConfigPayload(config);
    if (!validation.valid) {
      return Failure(
        appError(
          "validation",
          `Validation failed: ${validation.errors?.join(", ")}`,
        ),
      );
    }

    if (!config.runtime || !SUPPORTED_RUNTIMES.includes(config.runtime)) {
      return Failure(
        appError(
          "validation",
          `Bots must specify a valid runtime (${SUPPORTED_RUNTIMES.join(", ")})`,
        ),
      );
    }

    return Ok(null);
  }

  private async checkUserOwnsExistingBot(
    userId: string,
    name: string,
  ): Promise<Result<null, AppError>> {
    const existsResult = await this.customBotRepository.globalBotExists(name);
    if (!existsResult.success) {
      return Failure(existsResult.error);
    }

    if (!existsResult.data) {
      return Failure(
        appError(
          "not_found",
          "Custom bot does not exist. Create it first using POST.",
        ),
      );
    }

    const ownershipCheckResult =
      await this.customBotRepository.checkUserOwnership(userId, name);
    if (!ownershipCheckResult.success) {
      return Failure(ownershipCheckResult.error);
    }

    return Ok(null);
  }

  private async checkVersionIsNew(
    name: string,
    version: string,
  ): Promise<Result<null, AppError>> {
    const latestResult =
      await this.customBotRepository.getGlobalLatestVersion(name);
    if (!latestResult.success) {
      return Failure(latestResult.error);
    }

    const latestVersion = latestResult.data.version;
    if (!semver.gt(version, latestVersion)) {
      return Failure(
        appError(
          "validation",
          `Version ${version} must be greater than current version ${latestVersion}`,
        ),
      );
    }

    const versionExistsResult =
      await this.customBotRepository.globalVersionExists(name, version);
    if (!versionExistsResult.success) {
      return Failure(versionExistsResult.error);
    }

    if (versionExistsResult.data) {
      return Failure(
        appError(
          "validation",
          `Version ${version} already exists for this bot`,
        ),
      );
    }

    return Ok(null);
  }

  async getUserCustomBots(
    userId: string,
  ): Promise<Result<CustomBotWithVersions[], AppError>> {
    try {
      const result = await this.customBotRepository.getUserCustomBots(userId);

      if (!result.success) {
        return Failure(result.error);
      }

      return result;
    } catch (error: unknown) {
      this.logger.error({ err: error }, "Error getting user custom bots");
      return Failure(
        appError(
          "internal",
          `Failed to get user custom bots: ${errorMessage(error)}`,
        ),
      );
    }
  }

  async getAllGlobalVersions(
    name: string,
  ): Promise<Result<CustomBotWithVersions, AppError>> {
    return await this.customBotRepository.getAllGlobalVersions(name);
  }

  async getGlobalSpecificVersion(
    name: string,
    version: string,
  ): Promise<Result<CustomBot, AppError>> {
    return await this.customBotRepository.findGlobalByKeyAndVersion(
      name,
      version,
    );
  }

  async getGlobalLatestVersion(
    name: string,
  ): Promise<Result<CustomBot, AppError>> {
    return await this.customBotRepository.getGlobalLatestVersion(name);
  }

  async getAllGlobalCustomBots(): Promise<
    Result<CustomBotWithVersions[], AppError>
  > {
    return await this.customBotRepository.getAllGlobalCustomBots();
  }

  async getById(id: string): Promise<Result<CustomBot, AppError>> {
    return await this.customBotRepository.findOneById(id);
  }

  async getVersionsWithInstanceCounts(
    userId: string,
    name: string,
  ): Promise<Result<VersionWithInstances[], AppError>> {
    const versionsResult = await this.customBotRepository.getAllUserVersions(
      userId,
      name,
    );
    if (!versionsResult.success) {
      return Failure(versionsResult.error);
    }

    const ids = versionsResult.data.versions.map((v) => v.id);
    const countsResult =
      await this.customBotRepository.countInstancesByCustomBotIds(ids);
    if (!countsResult.success) {
      return Failure(countsResult.error);
    }

    const enriched: VersionWithInstances[] = versionsResult.data.versions.map(
      (v) => ({
        ...v,
        instanceCount: countsResult.data.get(v.id) || 0,
      }),
    );

    return Ok(enriched);
  }

  async deleteVersion(
    userId: string,
    name: string,
    version: string,
  ): Promise<Result<void, AppError>> {
    const ownershipResult = await this.customBotRepository.checkUserOwnership(
      userId,
      name,
    );
    if (!ownershipResult.success) {
      return Failure(ownershipResult.error);
    }

    const versionResult =
      await this.customBotRepository.findGlobalByKeyAndVersion(name, version);
    if (!versionResult.success) {
      return Failure(versionResult.error);
    }

    const customBot = versionResult.data;
    const countResult =
      await this.customBotRepository.countInstancesByCustomBotId(customBot.id);
    if (!countResult.success) {
      return Failure(countResult.error);
    }

    if (countResult.data > 0) {
      return Failure(
        appError(
          "validation",
          `Cannot delete version ${version}: ${countResult.data} active instance(s) reference it`,
        ),
      );
    }

    // Delete DB row first, then best-effort storage cleanup.
    // Orphaned MinIO files are harmless; dangling DB rows pointing to
    // missing files would be a real problem.
    const removeResult = await this.customBotRepository.removeVersion(
      userId,
      customBot.id,
    );
    if (!removeResult.success) {
      if (removeResult.error.reason === "error") {
        this.logger.error(
          { name, error: removeResult.error.message },
          "Failed to delete custom bot versions",
        );
      }
      return Failure(
        removeResult.error.reason === "in-use"
          ? appError(
              "validation",
              `Cannot delete version ${version}: active instance(s) reference it`,
            )
          : WRITE_FAILED,
      );
    }

    // Best-effort MinIO cleanup (code bundle + frontend)
    const filesResult = await this.storageService.listObjects(
      customBot.filePath,
    );
    if (!filesResult.success) {
      this.logger.warn(
        { filePath: customBot.filePath, error: filesResult.error },
        "Failed to list files for cleanup after version deletion",
      );
    } else {
      for (const filePath of filesResult.data) {
        const deleteResult = await this.storageService.deleteFile(filePath);
        if (!deleteResult.success) {
          this.logger.warn(
            { filePath, error: deleteResult.error },
            "Failed to delete file during version cleanup",
          );
        }
      }
    }

    return Ok(null);
  }

  async deleteAllVersions(
    userId: string,
    name: string,
  ): Promise<Result<void, AppError>> {
    const ownershipResult = await this.customBotRepository.checkUserOwnership(
      userId,
      name,
    );
    if (!ownershipResult.success) {
      return Failure(ownershipResult.error);
    }

    const versionsResult =
      await this.customBotRepository.getAllGlobalVersions(name);
    if (!versionsResult.success) {
      return Failure(versionsResult.error);
    }

    const ids = versionsResult.data.versions.map((v) => v.id);
    const countsResult =
      await this.customBotRepository.countInstancesByCustomBotIds(ids);
    if (!countsResult.success) {
      return Failure(countsResult.error);
    }

    const versionsWithInstances: string[] = [];
    for (const [id, count] of countsResult.data) {
      if (count > 0) {
        const v = versionsResult.data.versions.find((ver) => ver.id === id);
        versionsWithInstances.push(`${v?.version || id} (${count})`);
      }
    }

    if (versionsWithInstances.length > 0) {
      return Failure(
        appError(
          "validation",
          `Cannot delete: versions with active instance(s): ${versionsWithInstances.join(", ")}`,
        ),
      );
    }

    // Delete DB rows first, then best-effort storage cleanup
    const removeResult = await this.customBotRepository.removeAllByName(
      userId,
      name,
    );
    if (!removeResult.success) {
      if (removeResult.error.reason === "error") {
        this.logger.error(
          { name, error: removeResult.error.message },
          "Failed to delete custom bot versions",
        );
      }
      return Failure(
        removeResult.error.reason === "in-use"
          ? appError(
              "validation",
              "Cannot delete: versions with active instance(s)",
            )
          : WRITE_FAILED,
      );
    }

    // Best-effort MinIO cleanup for all versions
    const prefix = `${userId}/${name}/`;
    const filesResult = await this.storageService.listObjects(prefix);
    if (!filesResult.success) {
      this.logger.warn(
        { prefix, error: filesResult.error },
        "Failed to list files for cleanup after deleting all versions",
      );
    } else {
      for (const filePath of filesResult.data) {
        const deleteResult = await this.storageService.deleteFile(filePath);
        if (!deleteResult.success) {
          this.logger.warn(
            { filePath, error: deleteResult.error },
            "Failed to delete file during bulk version cleanup",
          );
        }
      }
    }

    return Ok(null);
  }

  async checkOrphaned(
    customBotId: string,
  ): Promise<
    Result<{ orphaned: boolean; name: string; version: string }, AppError>
  > {
    const botResult = await this.customBotRepository.findOneById(customBotId);
    if (!botResult.success) {
      return Failure(botResult.error);
    }

    const countResult =
      await this.customBotRepository.countInstancesByCustomBotId(customBotId);
    if (!countResult.success) {
      return Failure(countResult.error);
    }

    return Ok({
      orphaned: countResult.data === 0,
      name: botResult.data.name,
      version: botResult.data.version,
    });
  }
}

const invalid = <T>(message: string): Result<T, AppError> =>
  Failure(appError("validation", message));
