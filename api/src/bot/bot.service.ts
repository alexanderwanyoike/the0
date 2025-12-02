import { Inject, Injectable, Scope } from "@nestjs/common";
import { CreateBotDto } from "./dto/create-bot.dto";
import { UpdateBotDto } from "./dto/update-bot.dto";
import { BotRepository } from "./bot.repository";
import { REQUEST } from "@nestjs/core";
import { PinoLogger, InjectPinoLogger } from "nestjs-pino";
import { Bot } from "./entities/bot.entity";
import { Failure, Ok, Result } from "@/common/result";
import { BotValidator } from "./bot.validator";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { CustomBot, BotType } from "@/custom-bot/custom-bot.types";
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
    @Inject(REQUEST) private readonly request: any,
    private readonly botRepository: BotRepository,
    private readonly botValidator: BotValidator,
    private readonly customBotService: CustomBotService,
    // UserBotsService removed for OSS version
    private readonly natsService: NatsService,
    @InjectPinoLogger(BotService.name)
    private readonly logger: PinoLogger,
  ) {}

  async create(createBotDto: CreateBotDto): Promise<Result<Bot, string>> {
    const { uid } = this.request.user;

    const validationResult = await this.validateBotTypeAndConfig(
      createBotDto.config,
    );

    if (!validationResult.success) {
      return Failure<Bot, string>(
        validationResult.error || "Invalid bot config",
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
      return Failure<Bot, string>(deploymentAuthResult.error);
    }

    const result = await this.botRepository.create({
      ...createBotDto,
      userId: uid,
      topic: "the0-scheduled-custom-bot",
      customBotId: validationResult.data.id,
    });

    if (!result.success) {
      return Failure<Bot, string>(result.error);
    }

    // Fetch custom bot data and publish bot creation event (replaces event-handler service)
    const customBotResult = await this.customBotService.getUserSpecificVersion(
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

  async update(
    id: string,
    updateBotDto: UpdateBotDto,
  ): Promise<Result<Bot, string>> {
    const validationResult = await this.validateBotTypeAndConfig(
      updateBotDto.config,
    );
    if (!validationResult.success) {
      return Failure<Bot, string>(
        validationResult.error || "Invalid bot config",
      );
    }

    const { uid } = this.request.user;
    const result = await this.botRepository.update(uid, id, updateBotDto);

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

  async remove(id: string): Promise<Result<void, string>> {
    const { uid } = this.request.user;

    // Fetch bot data before deletion for event payload
    const botResult = await this.botRepository.findOne(uid, id);
    if (!botResult.success) {
      return Failure("Bot not found");
    }

    // Get custom bot data to determine correct topic
    let topics = null;
    if (botResult.data.config?.type && botResult.data.config?.version) {
      const [_, name] = botResult.data.config.type.split("/");
      const customBotResult =
        await this.customBotService.getUserSpecificVersion(
          uid,
          name,
          botResult.data.config.version,
        );

      if (customBotResult.success) {
        topics = this.getTopicsForBotType(customBotResult.data);
      }
    }

    const result = await this.botRepository.remove(uid, id);

    if (result.success && topics) {
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

    return result;
  }

  private async validateBotTypeAndConfig(
    config: any,
  ): Promise<Result<CustomBot, string>> {
    const { type, version } = config;

    if (!type) {
      return {
        success: false,
        error: "Bot type is required",
        data: null,
      };
    }

    if (!version) {
      return {
        success: false,
        error: "Bot version is required",
        data: null,
      };
    }

    // Check that type is in the correct format
    if (!BOT_TYPE_PATTERN.test(type)) {
      return {
        success: false,
        error: `Invalid bot type format. Expected format: type/name`,
        data: null,
      };
    }

    // use semver to check if version is valid
    if (!semver.valid(version)) {
      return {
        success: false,
        error: `Invalid version format. Expected format: x.y.z`,
        data: null,
      };
    }

    // Extract vendor, type, and name from the bot type
    const [_, name] = type.split("/");

    const customBotResult =
      await this.customBotService.getGlobalSpecificVersion(name, version);
    if (!customBotResult.success) {
      return {
        success: false,
        error: `Bot type '${type}' not found`,
        data: null,
      };
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
