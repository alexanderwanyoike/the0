import { Module } from "@nestjs/common";
import { BotStateService } from "./bot-state.service";
import { BotStateController } from "./bot-state.controller";
import { ConfigModule } from "@nestjs/config";
import { BotModule } from "@/bot/bot.module";
import { ApiKeyModule } from "@/api-key/api-key.module";
import { LoggerModule } from "@/logger/logger.module";

@Module({
  imports: [ConfigModule, BotModule, ApiKeyModule, LoggerModule],
  controllers: [BotStateController],
  providers: [BotStateService],
  exports: [BotStateService],
})
export class BotStateModule {}
