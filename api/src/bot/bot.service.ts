import { Inject, Injectable, Scope } from "@nestjs/common";
import { CreateBotDto } from "./dto/create-bot.dto";
import { UpdateBotDto } from "./dto/update-bot.dto";
import { BotRepository } from "./bot.repository";
import { REQUEST } from "@nestjs/core";
import { PinoLogger } from "nestjs-pino";
import { Bot } from "./entities/bot.entity";
import { Failure, Ok, Result } from "@/common/result";
import { BotValidator } from "./bot.validator";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { StorageService } from "@/custom-bot/storage.service";
import { CustomBot, BotType, BotDeleteResult } from "@/custom-bot/custom-bot.types";
import { AuthenticatedRequest } from "@/auth/auth.types";
import { BotConfig } from "@/database/schema/bots";
// UserBotsService removed for OSS version
import { NatsService } from "@/nats/nats.service";
import * as semver from "semver";
import {
  BOT_TYPE_PATTERN,
  BOT_TOPICS,
  SCHEDULED_BOT_TOPICS,
} from "@/bot/bot.constants";

@Injectable({ scope: Scope.REQUEST })
export class BotService {
  constructor(
    @Inject(REQUEST) private readonly request: AuthenticatedRequest,
    private readonly botRepository: BotRepository,
    private readonly botValidator: BotValidator,
    private readonly customBotService: CustomBotService,
    private readonly storageService: StorageService,
    // UserBotsService removed for OSS version
    private readonly natsService: NatsService,
    private readonly logger: PinoLogger,
  ) {}

  async create(createBotDto: CreateBotDto): Promise<Result<Bot, string>> {
    const { uid } = this.request.user;

    const validationResult = await this.validateBotTypeAndConfig(
      createBotDto.config,
    );

    if (!validationResult.success) {
      return Failure(
        validationResult.error,
      );
    }

    // Get bot type from config
    const botType = this.getBotTypeFromConfig(validationResult.data);

    // Feature gates removed for OSS version - allow all bot types

    // Check deployment authorization
    const deploymentAuthResult = await this.checkDeploymentAuthorization(
      uid,
      validationResult.data,
    );

    if (!deploymentAuthResult.success) {
      return Failure(deploymentAuthResult.error);
    }

    // Get hasFrontend flag from the validated custom bot
    const hasCustomFrontend =
      validationResult.data.config?.hasFrontend ?? false;

    const result = await this.botRepository.create({
      ...createBotDto,
      config: { ...createBotDto.config, hasFrontend: hasCustomFrontend },
      userId: uid,
      topic: "the0-scheduled-custom-bot",
      customBotId: validationResult.data.id,
    });

    if (!result.success) {
      return Failure(result.error);
    }

    // validationResult.data is the CustomBot from validateBotTypeAndConfig
    const customBot = validationResult.data;
    const topics = this.getTopicsForBotType(customBot);

    if (topics) {
      // Format event data for runtime subscriber (bot-scheduler expects flat structure)
      const eventPayload = {
        id: result.data.id,
        config: {
          ...createBotDto.config,
          customBotId: validationResult.data.id,
        },
        custom: {
          config: customBot.config,
          createdAt: customBot.createdAt,
          updatedAt: customBot.updatedAt,
          filePath: customBot.filePath || "",
          version: customBot.version,
        },
      };

      // Publish bot creation event to appropriate runtime service
      const publishResult = await this.natsService.publish(
        topics.CREATED,
        eventPayload,
      );
      if (!publishResult.success) {
        this.logger.error(
          { error: publishResult.error, botId: result.data.id },
          "Failed to publish bot creation event",
        );
      }
    }

    return this.botRepository.findOne(uid, result.data.id);
  }

  findAll(): Promise<Result<Bot[], string>> {
    const { uid } = this.request.user;
    return this.botRepository.findAll(uid);
  }

  findOne(id: string): Promise<Result<Bot, string>> {
    const { uid } = this.request.user;
    return this.botRepository.findOne(uid, id);
  }

  findOneByUserId(userId: string, id: string): Promise<Result<Bot, string>> {
    return this.botRepository.findOne(userId, id);
  }

  async update(
    id: string,
    updateBotDto: UpdateBotDto,
  ): Promise<Result<Bot, string>> {
    const validationResult = await this.validateBotTypeAndConfig(
      updateBotDto.config,
    );
    if (!validationResult.success) {
      return Failure(
        validationResult.error,
      );
    }

    const { uid } = this.request.user;

    // Block breaking (major) version changes — require delete/redeploy for those
    const currentBot = await this.botRepository.findOne(uid, id);
    if (!currentBot.success) {
      return Failure("Bot not found");
    }
    const currentVersion = currentBot.data.config?.version;
    const newVersion = updateBotDto.config?.version;
    if (
      currentVersion &&
      newVersion &&
      semver.major(currentVersion) !== semver.major(newVersion)
    ) {
      return Failure(
        `Major version upgrade (${currentVersion} -> ${newVersion}) requires delete and redeploy. ` +
          `In-place updates are only supported for minor/patch bumps to preserve state compatibility.`,
      );
    }

    // Only rotate customBotId when version actually changes
    const newCustomBot = validationResult.data;
    const hasCustomFrontend = newCustomBot.config?.hasFrontend ?? false;
    const versionChanged =
      currentVersion && newVersion
        ? !semver.eq(currentVersion, newVersion)
        : true;
    const result = await this.botRepository.update(uid, id, {
      ...updateBotDto,
      config: { ...updateBotDto.config, hasFrontend: hasCustomFrontend },
      customBotId: versionChanged
        ? newCustomBot.id
        : currentBot.data.customBotId,
    });

    if (result.success) {
      // Fetch updated bot and custom bot data for event payload
      const botResult = await this.botRepository.findOne(uid, id);
      if (botResult.success) {
        const customBotResult =
          await this.customBotService.getUserSpecificVersion(
            uid,
            validationResult.data.name,
            validationResult.data.version,
          );

        if (customBotResult.success) {
          const customBot = customBotResult.data;
          const topics = this.getTopicsForBotType(customBot);

          if (topics) {
            // Format event data for runtime subscriber (bot-scheduler expects flat structure)
            const eventPayload = {
              id: botResult.data.id,
              config: {
                ...updateBotDto.config,
                customBotId: validationResult.data.id,
              },
              custom: {
                config: customBot.config,
                createdAt: customBot.createdAt,
                updatedAt: customBot.updatedAt,
                filePath: customBot.filePath || "",
                version: customBot.version,
              },
            };

            // Publish bot update event to appropriate runtime service
            const publishResult = await this.natsService.publish(
              topics.UPDATED,
              eventPayload,
            );
            if (!publishResult.success) {
              this.logger.error(
                { error: publishResult.error, botId: botResult.data.id },
                "Failed to publish bot update event",
              );
            }
          }
        }
      }
    }

    return result;
  }

  async remove(id: string): Promise<Result<BotDeleteResult, string>> {
    const { uid } = this.request.user;

    // Fetch bot data before deletion for event payload
    const botResult = await this.botRepository.findOne(uid, id);
    if (!botResult.success) {
      return Failure("Bot not found");
    }

    // Get custom bot data to determine correct topic
    let topics = null;
    const configType = botResult.data.config?.type;
    const configVersion = botResult.data.config?.version;
    if (configType && configVersion) {
      const [_, name] = configType.split("/");
      if (!name?.trim()) {
        this.logger.warn(
          { type: configType },
          "Invalid bot type: missing name after '/'",
        );
        return Failure("Invalid bot type: missing name after '/'");
      }
      const customBotResult =
        await this.customBotService.getUserSpecificVersion(
          uid,
          name,
          configVersion,
        );

      if (customBotResult.success) {
        topics = this.getTopicsForBotType(customBotResult.data);
      }
    }

    const result = await this.botRepository.remove(uid, id);

    if (!result.success) {
      return Failure(result.error);
    }

    // Clean up bot logs from MinIO (fire-and-forget, don't fail the deletion)
    const logsCleanup = await this.storageService.deletePrefixFromBucket(
      "bot-logs",
      `logs/${botResult.data.id}/`,
    );
    if (!logsCleanup.success) {
      this.logger.warn(
        { error: logsCleanup.error, botId: botResult.data.id },
        "Failed to clean up bot logs",
      );
    }

    // Clean up bot state from MinIO
    const stateCleanup = await this.storageService.deletePrefixFromBucket(
      "bot-state",
      `${botResult.data.id}/`,
    );
    if (!stateCleanup.success) {
      this.logger.warn(
        { error: stateCleanup.error, botId: botResult.data.id },
        "Failed to clean up bot state",
      );
    }

    // Check if custom bot version is now orphaned
    const deleteResult: BotDeleteResult = {};
    if (botResult.data.customBotId) {
      const orphanResult = await this.customBotService.checkOrphaned(
        botResult.data.customBotId,
      );
      if (orphanResult.success && orphanResult.data.orphaned) {
        deleteResult.orphanedVersion = {
          name: orphanResult.data.name,
          version: orphanResult.data.version,
          customBotId: botResult.data.customBotId,
        };
      }
    }

    if (topics) {
      // Format event data for runtime subscriber (bot-scheduler expects flat structure)
      const eventPayload = {
        id: botResult.data.id,
        config: botResult.data.config,
      };

      // Publish bot deletion event to appropriate runtime service
      const publishResult = await this.natsService.publish(
        topics.DELETED,
        eventPayload,
      );
      if (!publishResult.success) {
        this.logger.error(
          { error: publishResult.error, botId: botResult.data.id },
          "Failed to publish bot deletion event",
        );
      }
    }

    return Ok(deleteResult);
  }

  private async validateBotTypeAndConfig(
    config: BotConfig,
  ): Promise<Result<CustomBot, string>> {
    const { type, version } = config;

    if (!type) {
      return Failure("Bot type is required");
    }

    if (!version) {
      return Failure("Bot version is required");
    }

    if (!BOT_TYPE_PATTERN.test(type)) {
      return Failure("Invalid bot type format. Expected format: type/name");
    }

    if (!semver.valid(version)) {
      return Failure("Invalid version format. Expected format: x.y.z");
    }

    const [_, name] = type.split("/");
    if (!name?.trim()) {
      return Failure("Invalid bot type: missing name after '/'");
    }

    const customBotResult =
      await this.customBotService.getGlobalSpecificVersion(name, version);
    if (!customBotResult.success) {
      return Failure(`Bot type '${type}' not found`);
    }

    const customBot = customBotResult.data;
    const validationResult = await this.botValidator.validate(
      config,
      customBot,
    );
    if (!validationResult.success) {
      return {
        success: false,
        error: validationResult.error.join(", ") || "Invalid bot config",
        data: null,
      };
    }

    return { success: true, error: null, data: customBot };
  }

  private async checkDeploymentAuthorization(
    userId: string,
    customBot: CustomBot,
  ): Promise<Result<void, string>> {
    // OSS version - all bots are deployable by their owners or anyone (free access)
    return { success: true, error: null, data: null };
  }

  private getBotTypeFromConfig(customBot: CustomBot): BotType {
    return customBot.config.type;
  }

  private getTopicsForBotType(
    customBot: CustomBot,
  ): { CREATED: string; UPDATED: string; DELETED: string } | null {
    const botType = customBot.config.type;

    if (botType === "realtime") {
      return BOT_TOPICS;
    } else if (botType === "scheduled") {
      return SCHEDULED_BOT_TOPICS;
    } else {
      this.logger.warn({ botType }, "Unknown bot type");
      return null;
    }
  }
}
