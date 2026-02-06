import { Module } from "@nestjs/common";
import { LogsService } from "./logs.service";
import { LogsController } from "./logs.controller";
import { ConfigModule } from "@nestjs/config";
import { BotModule } from "@/bot/bot.module";
import { ApiKeyModule } from "@/api-key/api-key.module";
import { LoggerModule } from "@/logger/logger.module";

@Module({
  imports: [ConfigModule, BotModule, ApiKeyModule, LoggerModule],
  controllers: [LogsController],
  providers: [LogsService],
  exports: [LogsService],
})
export class LogsModule {}
