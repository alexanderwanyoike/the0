import { Injectable } from "@nestjs/common";
import { PinoLogger } from "nestjs-pino";
import { CustomBotRepository } from "./custom-bot.repository";
import { StorageService } from "@/custom-bot/storage.service";
import { validateCustomBotConfigPayload } from "./custom-bot.schema";

import {
  CustomBotConfig,
  CustomBot,
  CustomBotWithVersions,
} from "./custom-bot.types";
import { Result, Failure, Ok } from "@/common/result";

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

      if (
        config.type === "realtime" &&
        (!config.runtime ||
          !["python3.11", "nodejs20"].includes(config.runtime))
      ) {
        return Failure(
          "Realtime bots must specify a valid runtime (python3.11 or nodejs20)",
        );
      }

      if (config.type === "scheduled" && config.runtime !== "python3.11") {
        return Failure(
          "Scheduled bots must use python3.11 runtime (nodejs20 is not supported for scheduled bots)",
        );
      }

      if (existsResult.data) {
        return Failure("Custom bot with this name already exists");
      }

      // Validate ZIP file structure from uploaded file
      const zipValidation = await this.storageService.validateZipStructure(
        filePath,
        Object.values(config.entrypoints).filter(Boolean),
      );
      if (!zipValidation.success) {
        return Failure(`ZIP validation failed: ${zipValidation.error}`);
      }

      // Extract frontend bundle if present (do this before creating bot record)
      const frontendResult = await this.storageService.extractAndStoreFrontend(
        filePath,
        userId,
        config.name,
        config.version,
      );

      // Update config with hasFrontend flag
      const finalConfig = {
        ...config,
        hasFrontend: frontendResult.success && frontendResult.data !== null,
      };

      if (frontendResult.success && frontendResult.data) {
        this.logger.info(
          { botName: config.name, frontendPath: frontendResult.data },
          "Frontend bundle extracted for bot",
        );
      }

      // Create the bot
      const botData: Partial<CustomBot> = {
        name: finalConfig.name,
        version: finalConfig.version,
        config: finalConfig,
        filePath: filePath,
        status: "active",
      };

      const result = await this.customBotRepository.createNewGlobalVersion(
        userId,
        botData,
      );

      if (!result.success) {
        return Failure(result.error);
      }

      return await this.customBotRepository.getSpecificGlobalVersion(
        config.name,
        config.version,
      );
    } catch (error: any) {
      this.logger.error({ err: error }, "Error creating custom bot");
      return Failure(`Failed to create custom bot: ${error.message}`);
    }
  }

  async updateCustomBot(
    userId: string,
    name: string,
    config: CustomBotConfig,
    filePath: string,
  ): Promise<Result<CustomBot, string>> {
    try {
      // Validate config structure
      const validation = validateCustomBotConfigPayload(config);
      if (!validation.valid) {
        return Failure(`Validation failed: ${validation.errors?.join(", ")}`);
      }

      // Ensure the name in config matches the parameter
      if (config.name !== name) {
        return Failure("Bot name in config must match the URL parameter");
      }

      if (
        config.type === "realtime" &&
        (!config.runtime ||
          !["python3.11", "nodejs20"].includes(config.runtime))
      ) {
        return Failure(
          "Realtime bots must specify a valid runtime (python3.11 or nodejs20)",
        );
      }

      if (config.type === "scheduled" && config.runtime !== "python3.11") {
        return Failure(
          "Scheduled bots must use python3.11 runtime (nodejs20 is not supported for scheduled bots)",
        );
      }

      // Check if bot exists
      const existsResult = await this.customBotRepository.globalBotExists(name);
      if (!existsResult.success) {
        return Failure(existsResult.error);
      }

      if (!existsResult.data) {
        return Failure(
          "Custom bot does not exist. Create it first using POST.",
        );
      }

      // Check if the user is the owner of the bot
      const ownershipCheckResult =
        await this.customBotRepository.checkUserOwnership(userId, name);
      if (!ownershipCheckResult.success) {
        return Failure(ownershipCheckResult.error);
      }

      // Get latest version to compare
      const latestResult =
        await this.customBotRepository.getGlobalLatestVersion(name);
      if (!latestResult.success) {
        return Failure(latestResult.error);
      }

      const latestBot = latestResult.data;

      // Check if new version is actually newer
      const isNewer = this.customBotRepository.isVersionNewer(
        latestBot.version,
        config.version,
      );
      if (!isNewer) {
        return Failure(
          `Version ${config.version} must be greater than current version ${latestBot.version}`,
        );
      }

      // Check if this exact version already exists
      const versionExistsResult =
        await this.customBotRepository.globalVersionExists(
          name,
          config.version,
        );
      if (!versionExistsResult.success) {
        return Failure(versionExistsResult.error);
      }

      if (versionExistsResult.data) {
        return Failure(`Version ${config.version} already exists for this bot`);
      }

      // Validate ZIP file structure from uploaded file
      const zipValidation = await this.storageService.validateZipStructure(
        filePath,
        Object.values(config.entrypoints).filter(Boolean),
      );
      if (!zipValidation.success) {
        return Failure(`ZIP validation failed: ${zipValidation.error}`);
      }

      // Extract frontend bundle if present
      const frontendResult = await this.storageService.extractAndStoreFrontend(
        filePath,
        userId,
        config.name,
        config.version,
      );

      // Update config with hasFrontend flag
      const finalConfig = {
        ...config,
        hasFrontend: frontendResult.success && frontendResult.data !== null,
      };

      if (frontendResult.success && frontendResult.data) {
        this.logger.info(
          { botName: config.name, frontendPath: frontendResult.data },
          "Frontend bundle extracted for bot update",
        );
      }

      // Create new version
      const botData: Partial<CustomBot> = {
        name: finalConfig.name,
        version: finalConfig.version,
        config: finalConfig,
        filePath: filePath,
        status: "active",
      };

      const customBot = await this.customBotRepository.createNewGlobalVersion(
        userId,
        botData,
      );

      if (!customBot.success) {
        return Failure(customBot.error);
      }

      return await this.customBotRepository.getSpecificGlobalVersion(
        config.name,
        config.version,
      );
    } catch (error: any) {
      this.logger.error({ err: error }, "Error updating custom bot");
      return Failure(`Failed to update custom bot: ${error.message}`);
    }
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
    } catch (error: any) {
      this.logger.error({ err: error }, "Error getting user custom bots");
      return Failure(`Failed to get user custom bots: ${error.message}`);
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
}
