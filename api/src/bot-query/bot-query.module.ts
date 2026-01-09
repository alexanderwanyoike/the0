import { Module } from "@nestjs/common";
import { BotQueryService } from "./bot-query.service";
import { BotQueryController } from "./bot-query.controller";
import { ConfigModule } from "@nestjs/config";
import { BotModule } from "@/bot/bot.module";
import { ApiKeyModule } from "@/api-key/api-key.module";
import { LoggerModule } from "@/logger/logger.module";

@Module({
  imports: [ConfigModule, BotModule, ApiKeyModule, LoggerModule],
  controllers: [BotQueryController],
  providers: [BotQueryService],
  exports: [BotQueryService],
})
export class BotQueryModule {}
