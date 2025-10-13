import { Injectable } from "@nestjs/common";
import { Bot } from "./entities/bot.entity";
import { RoleRepository } from "@/common/role.repository";

@Injectable()
export class BotRepository extends RoleRepository<Bot> {
  protected readonly tableName = "bots" as const;

  constructor() {
    super();
  }
}
