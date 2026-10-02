import { Injectable } from "@nestjs/common";
import { PinoLogger } from "nestjs-pino";
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
import { Result, Failure, Ok, errorMessage } from "@/common/result";

@Injectable()
export class CustomBotService {
  constructor(
    private readonly customBotRepository: CustomBotRepository,
    private readonly storageService: StorageService,
    private readonly logger: PinoLogger,
  ) {}

  async createCustomBot(
    userId: string,
    config: CustomBotConfig,
    filePath: string,
  ): Promise<Result<CustomBot, string>> {
    try {
      // Validate config structure
      const validation = validateCustomBotConfigPayload(config);
      if (!validation.valid) {
        return Failure(`Validation failed: ${validation.errors?.join(", ")}`);
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
          `Bots must specify a valid runtime (${SUPPORTED_RUNTIMES.join(", ")})`,
        );
      }

      if (existsResult.data) {
        return Failure("Custom bot with this name already exists");
      }

      return await this.storeVersion(
        userId,
        config,
        filePath,
        "Frontend bundle extracted for bot",
      );
    } catch (error: unknown) {
      this.logger.error({ err: error }, "Error creating custom bot");
      return Failure(`Failed to create custom bot: ${errorMessage(error)}`);
    }
  }

  private async storeVersion(
    userId: string,
    config: CustomBotConfig,
    filePath: string,
    frontendLogMessage: string,
  ): Promise<Result<CustomBot, string>> {
    const requiredFiles = COMPILED_RUNTIMES.includes(config.runtime)
      ? []
      : Object.values(config.entrypoints).filter(Boolean);
    const zipValidation = await this.storageService.validateZipStructure(
      filePath,
      requiredFiles,
    );
    if (!zipValidation.success) {
      return Failure(`ZIP validation failed: ${zipValidation.error}`);
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

    const result = await this.customBotRepository.createNewGlobalVersion(
      userId,
      {
        name: finalConfig.name,
        version: finalConfig.version,
        config: finalConfig,
        filePath,
        status: "active",
      },
    );
    if (!result.success) {
      return Failure(result.error);
    }

    return this.customBotRepository.getSpecificGlobalVersion(
      config.name,
      config.version,
    );
  }

  async updateCustomBot(
    userId: string,
    name: string,
    config: CustomBotConfig,
    filePath: string,
  ): Promise<Result<CustomBot, string>> {
    try {
      const configCheck = this.checkUpdateConfig(name, config);
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
      return Failure(`Failed to update custom bot: ${errorMessage(error)}`);
    }
  }

  private checkUpdateConfig(
    name: string,
    config: CustomBotConfig,
  ): Result<null, string> {
    const validation = validateCustomBotConfigPayload(config);
    if (!validation.valid) {
      return Failure(`Validation failed: ${validation.errors?.join(", ")}`);
    }

    if (config.name !== name) {
      return Failure("Bot name in config must match the URL parameter");
    }

    if (!config.runtime || !SUPPORTED_RUNTIMES.includes(config.runtime)) {
      return Failure(
        `Bots must specify a valid runtime (${SUPPORTED_RUNTIMES.join(", ")})`,
      );
    }

    return Ok(null);
  }

  private async checkUserOwnsExistingBot(
    userId: string,
    name: string,
  ): Promise<Result<null, string>> {
    const existsResult = await this.customBotRepository.globalBotExists(name);
    if (!existsResult.success) {
      return Failure(existsResult.error);
    }

    if (!existsResult.data) {
      return Failure("Custom bot does not exist. Create it first using POST.");
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
  ): Promise<Result<null, string>> {
    const latestResult =
      await this.customBotRepository.getGlobalLatestVersion(name);
    if (!latestResult.success) {
      return Failure(latestResult.error);
    }

    const latestVersion = latestResult.data.version;
    if (!this.customBotRepository.isVersionNewer(latestVersion, version)) {
      return Failure(
        `Version ${version} must be greater than current version ${latestVersion}`,
      );
    }

    const versionExistsResult =
      await this.customBotRepository.globalVersionExists(name, version);
    if (!versionExistsResult.success) {
      return Failure(versionExistsResult.error);
    }

    if (versionExistsResult.data) {
      return Failure(`Version ${version} already exists for this bot`);
    }

    return Ok(null);
  }

  async getUserCustomBots(
    userId: string,
  ): Promise<Result<CustomBotWithVersions[], string>> {
    try {
      const result = await this.customBotRepository.getUserCustomBots(userId);

      if (!result.success) {
        return Failure(result.error);
      }

      return result;
    } catch (error: unknown) {
      this.logger.error({ err: error }, "Error getting user custom bots");
      return Failure(`Failed to get user custom bots: ${errorMessage(error)}`);
    }
  }

  async getAllUserVersions(
    userId: string,
    name: string,
  ): Promise<Result<CustomBotWithVersions, string>> {
    return await this.customBotRepository.getAllUserVersions(userId, name);
  }

  async getAllGlobalVersions(
    name: string,
  ): Promise<Result<CustomBotWithVersions, string>> {
    return await this.customBotRepository.getAllGlobalVersions(name);
  }

  async getUserSpecificVersion(
    userId: string,
    name: string,
    version: string,
  ): Promise<Result<CustomBot, string>> {
    return await this.customBotRepository.getSpecificUserVersion(
      userId,
      name,
      version,
    );
  }

  async getGlobalSpecificVersion(
    name: string,
    version: string,
  ): Promise<Result<CustomBot, string>> {
    return await this.customBotRepository.getSpecificGlobalVersion(
      name,
      version,
    );
  }

  async getGlobalLatestVersion(
    name: string,
  ): Promise<Result<CustomBot, string>> {
    return await this.customBotRepository.getGlobalLatestVersion(name);
  }

  async getAllGlobalCustomBots(): Promise<
    Result<CustomBotWithVersions[], string>
  > {
    return await this.customBotRepository.getAllGlobalCustomBots();
  }

  async getById(id: string): Promise<Result<CustomBot, string>> {
    return await this.customBotRepository.findOneById(id);
  }

  async getVersionsWithInstanceCounts(
    userId: string,
    name: string,
  ): Promise<Result<VersionWithInstances[], string>> {
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
  ): Promise<Result<void, string>> {
    const ownershipResult =
      await this.customBotRepository.checkUserOwnership(userId, name);
    if (!ownershipResult.success) {
      return Failure(ownershipResult.error);
    }

    const versionResult =
      await this.customBotRepository.getSpecificGlobalVersion(name, version);
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
        `Cannot delete version ${version}: ${countResult.data} active instance(s) reference it`,
      );
    }

    // Delete DB row first, then best-effort storage cleanup.
    // Orphaned MinIO files are harmless; dangling DB rows pointing to
    // missing files would be a real problem.
    const removeResult = await this.customBotRepository.remove(
      userId,
      customBot.id,
    );
    if (!removeResult.success) {
      return Failure(removeResult.error);
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
  ): Promise<Result<void, string>> {
    const ownershipResult =
      await this.customBotRepository.checkUserOwnership(userId, name);
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
        `Cannot delete: versions with active instance(s): ${versionsWithInstances.join(", ")}`,
      );
    }

    // Delete DB rows first, then best-effort storage cleanup
    const removeResult = await this.customBotRepository.removeAllByName(
      userId,
      name,
    );
    if (!removeResult.success) {
      return Failure(removeResult.error);
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
    Result<{ orphaned: boolean; name: string; version: string }, string>
  > {
    const botResult =
      await this.customBotRepository.findOneById(customBotId);
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
