import { Actor } from "@/auth/actor";
import { Command } from "@/command-bus/command";
import { AppError } from "@/common/app-error";
import { Result } from "@/common/result";
import { BotDeleteResult } from "@/custom-bot/custom-bot.types";
import { BotConfig } from "@/database/schema/bots";
import { Bot } from "../entities/bot.entity";

/** Deploys a new Bot for the Actor's user. */
export class DeployBot extends Command<Result<Bot, AppError>> {
  constructor(
    readonly actor: Actor,
    readonly bot: { name: string; config: BotConfig },
  ) {
    super();
  }
}

/** Replaces the config, and optionally the name, of one of the Actor's Bots. */
export class UpdateBot extends Command<Result<Bot, AppError>> {
  constructor(
    readonly actor: Actor,
    readonly botId: string,
    readonly changes: { name?: string; config: BotConfig },
  ) {
    super();
  }
}

/** Deletes one of the Actor's Bots. */
export class DeleteBot extends Command<Result<BotDeleteResult, AppError>> {
  constructor(
    readonly actor: Actor,
    readonly botId: string,
  ) {
    super();
  }
}
