import { Injectable } from "@nestjs/common";
import { PinoLogger } from "nestjs-pino";
import { Failure, Ok, Result } from "@/common/result";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { CustomBot } from "@/custom-bot/custom-bot.types";
import { BotConfig } from "@/database/schema/bots";
import { BOT_TOPICS, SCHEDULED_BOT_TOPICS } from "../bot.constants";
import { BotEvent } from "../bot-event-outbox";
import { Bot } from "../entities/bot.entity";
import { customBotNameOf } from "./custom-bot-name";

type BotTopics = { CREATED: string; UPDATED: string; DELETED: string };

/** Builds the events the runtime receives for each Bot command. */
@Injectable()
export class BotEventFactory {
  constructor(
    private readonly customBotService: CustomBotService,
    private readonly logger: PinoLogger,
  ) {}

  created(botId: string, config: BotConfig, customBot: CustomBot): BotEvent[] {
    return botEvents(
      botId,
      this.topicsFor(customBot)?.CREATED,
      customBotEventPayload(botId, config, customBot.id, customBot),
    );
  }

  /**
   * The event carries the user's copy of the custom bot version, while
   * customBotId stays the globally validated one.
   */
  async updated(
    userId: string,
    botId: string,
    config: BotConfig,
    newCustomBot: CustomBot,
  ): Promise<BotEvent[]> {
    const customBotResult = await this.customBotService.getUserSpecificVersion(
      userId,
      newCustomBot.name,
      newCustomBot.version,
    );
    if (!customBotResult.success) return [];

    const customBot = customBotResult.data;
    return botEvents(
      botId,
      this.topicsFor(customBot)?.UPDATED,
      customBotEventPayload(botId, config, newCustomBot.id, customBot),
    );
  }

  /**
   * Only a malformed type blocks deletion. A bot with no type/version, or
   * whose custom bot version is gone, is still deleted, just without an
   * event.
   */
  async deleted(userId: string, bot: Bot): Promise<Result<BotEvent[], string>> {
    const configType = bot.config?.type;
    const configVersion = bot.config?.version;
    if (!configType || !configVersion) return Ok([]);

    const name = customBotNameOf(configType);
    if (!name.success) {
      this.logger.warn({ type: configType }, name.error);
      return Failure(name.error);
    }

    const customBotResult = await this.customBotService.getUserSpecificVersion(
      userId,
      name.data,
      configVersion,
    );
    if (!customBotResult.success) return Ok([]);

    return Ok(
      botEvents(bot.id, this.topicsFor(customBotResult.data)?.DELETED, {
        id: bot.id,
        config: bot.config,
      }),
    );
  }

  private topicsFor(customBot: CustomBot): BotTopics | null {
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

/** No subject means the runtime is not told about the change. */
const botEvents = (
  botId: string,
  subject: string | undefined,
  payload: Record<string, unknown>,
): BotEvent[] => (subject ? [{ botId, subject, payload }] : []);

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
