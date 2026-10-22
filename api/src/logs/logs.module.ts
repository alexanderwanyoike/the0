import { Module } from "@nestjs/common";
import { LogsService } from "./logs.service";
import { LogsController } from "./logs.controller";
import { LogStreamService } from "./log-stream.service";
import { LogFanOut } from "./log-fan-out";
import { ConfigModule } from "@nestjs/config";
import { BotModule } from "@/bot/bot.module";
import { ApiKeyModule } from "@/api-key/api-key.module";
import { LoggerModule } from "@/logger/logger.module";
import { NatsModule } from "@/nats/nats.module";

@Module({
  imports: [ConfigModule, BotModule, ApiKeyModule, LoggerModule, NatsModule],
  controllers: [LogsController],
  providers: [LogsService, LogStreamService, LogFanOut],
  exports: [LogsService],
})
export class LogsModule {}
