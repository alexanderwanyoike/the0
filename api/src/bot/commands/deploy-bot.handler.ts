import { Injectable, OnModuleInit } from "@nestjs/common";
import { createId } from "@paralleldrive/cuid2";
import { CommandBus } from "@/command-bus/command-bus";
import { CommandHandler } from "@/command-bus/command";
import { Failure, Result } from "@/common/result";
import { BotRepository } from "../bot.repository";
import { BotEventRelay } from "../bot-event-relay";
import { Bot } from "../entities/bot.entity";
import { DeployBot } from "./bot-commands";
import { BotConfigResolver } from "./bot-config.resolver";
import { BotEventFactory } from "./bot-event-factory";

@Injectable()
export class DeployBotHandler
  implements CommandHandler<DeployBot>, OnModuleInit
{
  constructor(
    private readonly commandBus: CommandBus,
    private readonly bots: BotRepository,
    private readonly configs: BotConfigResolver,
    private readonly events: BotEventFactory,
    private readonly relay: BotEventRelay,
  ) {}

  onModuleInit(): void {
    this.commandBus.register(DeployBot, this);
  }

  async execute({ actor, bot }: DeployBot): Promise<Result<Bot, string>> {
    const resolved = await this.configs.resolve(bot.config);
    if (!resolved.success) {
      return Failure(resolved.error);
    }
    const customBot = resolved.data;

    const id = createId();
    const events = this.events.created(id, bot.config, customBot);
    const result = await this.bots.createWithEvents(
      {
        ...bot,
        id,
        config: {
          ...bot.config,
          hasFrontend: customBot.config?.hasFrontend ?? false,
        },
        userId: actor.userId,
        topic: "the0-scheduled-custom-bot",
        customBotId: customBot.id,
      },
      events,
    );
    if (!result.success) {
      return Failure(result.error);
    }

    if (events.length > 0) void this.relay.publishPending();
    return this.bots.findOne(actor.userId, id);
  }
}
