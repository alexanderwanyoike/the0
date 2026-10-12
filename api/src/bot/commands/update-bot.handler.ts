import { Injectable, OnModuleInit } from "@nestjs/common";
import * as semver from "semver";
import { CommandBus } from "@/command-bus/command-bus";
import { CommandHandler } from "@/command-bus/command";
import { Failure, Result } from "@/common/result";
import { BotRepository } from "../bot.repository";
import { BotEventRelay } from "../bot-event-relay";
import { Bot } from "../entities/bot.entity";
import { UpdateBot } from "./bot-commands";
import { BotConfigResolver } from "./bot-config.resolver";
import { BotEventFactory } from "./bot-event-factory";

@Injectable()
export class UpdateBotHandler
  implements CommandHandler<UpdateBot>, OnModuleInit
{
  constructor(
    private readonly commandBus: CommandBus,
    private readonly bots: BotRepository,
    private readonly configs: BotConfigResolver,
    private readonly events: BotEventFactory,
    private readonly relay: BotEventRelay,
  ) {}

  onModuleInit(): void {
    this.commandBus.register(UpdateBot, this);
  }

  async execute({
    actor,
    botId,
    changes,
  }: UpdateBot): Promise<Result<Bot, string>> {
    const resolved = await this.configs.resolve(changes.config);
    if (!resolved.success) {
      return Failure(resolved.error);
    }

    const currentBot = await this.bots.findOne(actor.userId, botId);
    if (!currentBot.success) {
      return Failure("Bot not found");
    }
    const currentVersion = currentBot.data.config?.version;
    const newVersion = changes.config?.version;
    const majorChangeError = majorVersionChangeError(
      currentVersion,
      newVersion,
    );
    if (majorChangeError) {
      return Failure(majorChangeError);
    }

    const newCustomBot = resolved.data;
    const versionChanged =
      currentVersion && newVersion
        ? !semver.eq(currentVersion, newVersion)
        : true;
    const events = this.events.updated(botId, changes.config, newCustomBot);
    const result = await this.bots.updateWithEvents(
      actor.userId,
      botId,
      {
        ...changes,
        config: {
          ...changes.config,
          hasFrontend: newCustomBot.config?.hasFrontend ?? false,
        },
        customBotId: versionChanged
          ? newCustomBot.id
          : currentBot.data.customBotId,
      },
      events,
    );

    if (result.success) this.relay.publishCommitted(events);
    return result;
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
