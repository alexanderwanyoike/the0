import { Injectable } from "@nestjs/common";
import { BotRepository } from "./bot.repository";
import { Bot } from "./entities/bot.entity";
import { Result } from "@/common/result";
import { Actor } from "@/auth/actor";

/** Reads Bots. Changes go through the Bot commands on the CommandBus. */
@Injectable()
export class BotService {
  constructor(private readonly botRepository: BotRepository) {}

  findAll(actor: Actor): Promise<Result<Bot[], string>> {
    return this.botRepository.findAll(actor.userId);
  }

  findOne(actor: Actor, id: string): Promise<Result<Bot, string>> {
    return this.botRepository.findOne(actor.userId, id);
  }

  findOneByUserId(userId: string, id: string): Promise<Result<Bot, string>> {
    return this.botRepository.findOne(userId, id);
  }
}
