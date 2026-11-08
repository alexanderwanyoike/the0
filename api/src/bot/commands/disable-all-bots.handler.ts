import { Injectable, OnModuleInit } from "@nestjs/common";
import { CommandBus } from "@/command-bus/command-bus";
import { CommandHandler } from "@/command-bus/command";
import { appError, AppError } from "@/common/app-error";
import { Failure, Ok, Result } from "@/common/result";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { CustomBot } from "@/custom-bot/custom-bot.types";
import { BotRepository } from "../bot.repository";
import { BotEventRelay } from "../bot-event-relay";
import { Bot } from "../entities/bot.entity";
import { BotRef, DisableAllBots, DisabledBots } from "./bot-commands";
import { BotEventFactory } from "./bot-event-factory";

@Injectable()
export class DisableAllBotsHandler
  implements CommandHandler<DisableAllBots>, OnModuleInit
{
  constructor(
    private readonly commandBus: CommandBus,
    private readonly bots: BotRepository,
    private readonly customBotService: CustomBotService,
    private readonly events: BotEventFactory,
    private readonly relay: BotEventRelay,
  ) {}

  onModuleInit(): void {
    this.commandBus.register(DisableAllBots, this);
  }

  async execute({
    actor,
  }: DisableAllBots): Promise<Result<DisabledBots, AppError>> {
    const owned = await this.bots.findAll(actor.userId);
    if (!owned.success) return Failure(owned.error);

    // Loaded before the transaction, because the update event carries each
    // bot's Custom bot and the events are built while its rows are locked.
    const customBots = new Map<string, CustomBot>();
    for (const id of new Set(owned.data.map((bot) => bot.customBotId))) {
      const customBot = await this.customBotService.findVersionById(id);
      if (!customBot.success) return Failure(customBot.error);
      customBots.set(id, customBot.data);
    }

    const result = await this.bots.disableAllWithEvents(actor.userId, (bot) => {
      const customBot = customBots.get(bot.customBotId);
      return customBot
        ? Ok(this.events.updated(bot.id, bot.config, customBot))
        : Failure(BOTS_CHANGED);
    });
    if (!result.success) return Failure(result.error);

    this.relay.publishCommitted(result.data.events);
    return Ok({
      disabled: result.data.disabled.map(botRef),
      alreadyDisabled: result.data.alreadyDisabled.map(botRef),
    });
  }
}

/**
 * A bot deployed or moved to another version after the Custom bots were
 * loaded has none to announce it with. Nothing is disabled, and a rerun
 * loads it.
 */
const BOTS_CHANGED = appError(
  "conflict",
  "Your bots changed while they were being disabled, so none was disabled. Run the command again.",
);

const botRef = ({ id, name }: Bot): BotRef => ({ id, name });
