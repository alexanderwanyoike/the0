import { Injectable } from "@nestjs/common";
import { CreateBotDto } from "./dto/create-bot.dto";
import { UpdateBotDto } from "./dto/update-bot.dto";
import { BotRepository } from "./bot.repository";
import { PinoLogger } from "nestjs-pino";
import { Bot } from "./entities/bot.entity";
import { Failure, Ok, Result } from "@/common/result";
import { BotValidator } from "./bot.validator";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { CustomBot, BotType, BotDeleteResult } from "@/custom-bot/custom-bot.types";
import { Actor } from "@/auth/actor";
import { BotConfig } from "@/database/schema/bots";
// UserBotsService removed for OSS version
import * as semver from "semver";
import { createId } from "@paralleldrive/cuid2";
import { BotEvent } from "./bot-event-outbox";
import { BotEventRelay } from "./bot-event-relay";
import {
  BOT_TYPE_PATTERN,
  BOT_TOPICS,
  SCHEDULED_BOT_TOPICS,
} from "@/bot/bot.constants";

type BotTopics = { CREATED: string; UPDATED: string; DELETED: string };

@Injectable()
export class BotService {
  constructor(
    private readonly botRepository: BotRepository,
    private readonly botValidator: BotValidator,
    private readonly customBotService: CustomBotService,
    // UserBotsService removed for OSS version
    private readonly relay: BotEventRelay,
    private readonly logger: PinoLogger,
  ) {}

  async create(
    actor: Actor,
    createBotDto: CreateBotDto,
  ): Promise<Result<Bot, string>> {
    const uid = actor.userId;

    const validationResult = await this.validateBotTypeAndConfig(
      createBotDto.config,
    );

    if (!validationResult.success) {
      return Failure(validationResult.error);
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

    const id = createId();
    const events = this.createdEvents(
      id,
      createBotDto.config,
      validationResult.data,
    );
    const result = await this.botRepository.createWithEvents(
      {
        ...createBotDto,
        id,
        config: { ...createBotDto.config, hasFrontend: hasCustomFrontend },
        userId: uid,
        topic: "the0-scheduled-custom-bot",
        customBotId: validationResult.data.id,
      },
      events,
    );

    if (!result.success) {
      return Failure(result.error);
    }

    this.announce(events);
    return this.botRepository.findOne(uid, id);
  }

  private createdEvents(
    botId: string,
    config: BotConfig,
    customBot: CustomBot,
  ): BotEvent[] {
    const topics = this.getTopicsForBotType(customBot);
    if (!topics) return [];

    return [
      {
        botId,
        subject: topics.CREATED,
        payload: customBotEventPayload(botId, config, customBot.id, customBot),
      },
    ];
  }

  /**
   * The relay does not poll an idle outbox, so it must be told about newly
   * committed events. The command does not wait for JetStream.
   */
  private announce(events: BotEvent[]): void {
    if (events.length > 0) void this.relay.publishPending();
  }

  findAll(actor: Actor): Promise<Result<Bot[], string>> {
    return this.botRepository.findAll(actor.userId);
  }

  findOne(actor: Actor, id: string): Promise<Result<Bot, string>> {
    return this.botRepository.findOne(actor.userId, id);
  }

  findOneByUserId(userId: string, id: string): Promise<Result<Bot, string>> {
    return this.botRepository.findOne(userId, id);
  }

  async update(
    actor: Actor,
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

    const uid = actor.userId;

    const currentBot = await this.botRepository.findOne(uid, id);
    if (!currentBot.success) {
      return Failure("Bot not found");
    }
    const currentVersion = currentBot.data.config?.version;
    const newVersion = updateBotDto.config?.version;
    const majorChangeError = majorVersionChangeError(currentVersion, newVersion);
    if (majorChangeError) {
      return Failure(majorChangeError);
    }

    // Only rotate customBotId when version actually changes
    const newCustomBot = validationResult.data;
    const hasCustomFrontend = newCustomBot.config?.hasFrontend ?? false;
    const versionChanged =
      currentVersion && newVersion
        ? !semver.eq(currentVersion, newVersion)
        : true;
    const events = await this.updatedEvents(
      uid,
      id,
      updateBotDto.config,
      newCustomBot,
    );
    const result = await this.botRepository.updateWithEvents(
      uid,
      id,
      {
        ...updateBotDto,
        config: { ...updateBotDto.config, hasFrontend: hasCustomFrontend },
        customBotId: versionChanged
          ? newCustomBot.id
          : currentBot.data.customBotId,
      },
      events,
    );

    if (result.success) {
      this.announce(events);
    }

    return result;
  }

  /**
   * The event carries the user's copy of the custom bot version, while
   * customBotId stays the globally validated one.
   */
  private async updatedEvents(
    uid: string,
    id: string,
    config: BotConfig,
    newCustomBot: CustomBot,
  ): Promise<BotEvent[]> {
    const customBotResult = await this.customBotService.getUserSpecificVersion(
      uid,
      newCustomBot.name,
      newCustomBot.version,
    );
    if (!customBotResult.success) return [];

    const customBot = customBotResult.data;
    const topics = this.getTopicsForBotType(customBot);
    if (!topics) return [];

    return [
      {
        botId: id,
        subject: topics.UPDATED,
        payload: customBotEventPayload(id, config, newCustomBot.id, customBot),
      },
    ];
  }

  async remove(
    actor: Actor,
    id: string,
  ): Promise<Result<BotDeleteResult, string>> {
    const uid = actor.userId;

    // Fetch bot data before deletion for event payload
    const botResult = await this.botRepository.findOne(uid, id);
    if (!botResult.success) {
      return Failure("Bot not found");
    }

    const topicsResult = await this.topicsForExistingBot(uid, botResult.data);
    if (!topicsResult.success) {
      return Failure(topicsResult.error);
    }
    const topics = topicsResult.data;

    const events: BotEvent[] = topics
      ? [
          {
            botId: botResult.data.id,
            subject: topics.DELETED,
            payload: { id: botResult.data.id, config: botResult.data.config },
          },
        ]
      : [];
    const result = await this.botRepository.removeWithEvents(uid, id, events);

    if (!result.success) {
      return Failure(result.error);
    }
    this.announce(events);

    // Note: MinIO log/state cleanup is handled by the runtime GC process,
    // not here. The GC periodically sweeps orphaned artifacts after the
    // bot container has fully exited and done its final sync.

    const deleteResult = await this.describeOrphanedVersion(botResult.data);

    return Ok(deleteResult);
  }

  /**
   * Only a malformed type blocks deletion. A bot with no type/version, or
   * whose custom bot version is gone, is still deleted, just without an
   * event (null topics).
   */
  private async topicsForExistingBot(
    uid: string,
    bot: Bot,
  ): Promise<Result<BotTopics | null, string>> {
    const configType = bot.config?.type;
    const configVersion = bot.config?.version;
    if (!configType || !configVersion) return Ok(null);

    const [_, name] = configType.split("/");
    if (!name?.trim()) {
      this.logger.warn(
        { type: configType },
        "Invalid bot type: missing name after '/'",
      );
      return Failure("Invalid bot type: missing name after '/'");
    }

    const customBotResult = await this.customBotService.getUserSpecificVersion(
      uid,
      name,
      configVersion,
    );
    if (!customBotResult.success) return Ok(null);
    return Ok(this.getTopicsForBotType(customBotResult.data));
  }

  /** A failed orphan check is logged and reported as "not orphaned". */
  private async describeOrphanedVersion(bot: Bot): Promise<BotDeleteResult> {
    const deleteResult: BotDeleteResult = {};
    if (!bot.customBotId) return deleteResult;

    const orphanResult = await this.customBotService.checkOrphaned(
      bot.customBotId,
    );
    if (!orphanResult.success) {
      this.logger.warn(
        {
          botId: bot.id,
          customBotId: bot.customBotId,
          error: orphanResult.error,
        },
        "Failed to check if custom bot version is orphaned",
      );
    } else if (orphanResult.data.orphaned) {
      deleteResult.orphanedVersion = {
        name: orphanResult.data.name,
        version: orphanResult.data.version,
        customBotId: bot.customBotId,
      };
    }
    return deleteResult;
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
  ): BotTopics | null {
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

const majorVersionChangeError = (
  currentVersion: string | undefined,
  newVersion: string | undefined,
): string | null => {
  if (
    currentVersion &&
    newVersion &&
    semver.major(currentVersion) !== semver.major(newVersion)
  ) {
    return (
      `Major version upgrade (${currentVersion} -> ${newVersion}) requires delete and redeploy. ` +
      `In-place updates are only supported for minor/patch bumps to preserve state compatibility.`
    );
  }
  return null;
};

// The runtime's bot-scheduler subscriber expects this flat structure.
const customBotEventPayload = (
  botId: string,
  config: BotConfig,
  customBotId: string,
  customBot: CustomBot,
): Record<string, unknown> => ({
  id: botId,
  config: { ...config, customBotId },
  custom: {
    config: customBot.config,
    createdAt: customBot.createdAt,
    updatedAt: customBot.updatedAt,
    filePath: customBot.filePath || "",
    version: customBot.version,
  },
});
