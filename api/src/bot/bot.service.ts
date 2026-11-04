import { Injectable } from "@nestjs/common";
import { BotRepository } from "./bot.repository";
import { Bot } from "./entities/bot.entity";
import { appError, AppError } from "@/common/app-error";
import { Result } from "@/common/result";
import { Actor } from "@/auth/actor";

/** Reads Bots. Changes go through the Bot commands on the CommandBus. */
@Injectable()
export class BotService {
  constructor(private readonly botRepository: BotRepository) {}

  findAll(actor: Actor): Promise<Result<Bot[], AppError>> {
    return this.botRepository.findAll(actor.userId);
  }

  findOne(actor: Actor, id: string): Promise<Result<Bot, AppError>> {
    return this.botRepository.findOne(actor.userId, id);
  }

  findOneByUserId(userId: string, id: string): Promise<Result<Bot, AppError>> {
    return this.botRepository.findOne(userId, id);
  }
}

/**
 * A bot lookup that found nothing becomes the refusal readers of a Bot's
 * logs, state and queries see, which never says whether the bot exists.
 * Any other failure is passed on, so an outage is not reported as a refusal.
 */
export const botAccessError = (error: AppError): AppError =>
  error.code === "not_found"
    ? appError("not_found", "Bot not found or access denied")
    : error;
