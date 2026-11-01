import { Injectable, OnModuleInit } from "@nestjs/common";
import { PinoLogger } from "nestjs-pino";
import { CommandBus } from "@/command-bus/command-bus";
import { CommandHandler } from "@/command-bus/command";
import { AppError } from "@/common/app-error";
import { Failure, Ok, Result } from "@/common/result";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { BotDeleteResult } from "@/custom-bot/custom-bot.types";
import { BotRepository } from "../bot.repository";
import { BotEventRelay } from "../bot-event-relay";
import { Bot } from "../entities/bot.entity";
import { DeleteBot } from "./bot-commands";
import { BotEventFactory } from "./bot-event-factory";
import { ownedBotReadError } from "./owned-bot-read-error";

@Injectable()
export class DeleteBotHandler
  implements CommandHandler<DeleteBot>, OnModuleInit
{
  constructor(
    private readonly commandBus: CommandBus,
    private readonly bots: BotRepository,
    private readonly events: BotEventFactory,
    private readonly relay: BotEventRelay,
    private readonly customBotService: CustomBotService,
    private readonly logger: PinoLogger,
  ) {}

  onModuleInit(): void {
    this.commandBus.register(DeleteBot, this);
  }

  async execute({
    actor,
    botId,
  }: DeleteBot): Promise<Result<BotDeleteResult, AppError>> {
    // Read first: the delete event carries the config the bot was stored with.
    const botResult = await this.bots.findOne(actor.userId, botId);
    if (!botResult.success) {
      return Failure(ownedBotReadError(botResult.error));
    }

    const eventsResult = await this.events.deleted(botResult.data);
    if (!eventsResult.success) {
      return Failure(eventsResult.error);
    }
    const events = eventsResult.data;

    const result = await this.bots.removeWithEvents(
      actor.userId,
      botId,
      events,
    );
    if (!result.success) {
      return Failure(result.error);
    }
    this.relay.publishCommitted(events);

    // Note: MinIO log/state cleanup is handled by the runtime GC process,
    // not here. The GC periodically sweeps orphaned artifacts after the
    // bot container has fully exited and done its final sync.

    return Ok(await this.describeOrphanedVersion(botResult.data));
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
}
