import { Module } from "@nestjs/common";
import { McpController } from "./mcp.controller";
import { McpService } from "./mcp.service";
import { BotModule } from "@/bot/bot.module";
import { CustomBotModule } from "@/custom-bot/custom-bot.module";
import { LogsModule } from "@/logs/logs.module";
import { ApiKeyModule } from "@/api-key/api-key.module";
import { LoggerModule } from "@/logger/logger.module";
import { BotStateModule } from "@/bot-state/bot-state.module";
import { BotQueryModule } from "@/bot-query/bot-query.module";
import { CommandBusModule } from "@/command-bus/command-bus.module";

@Module({
  imports: [
    BotModule,
    CustomBotModule,
    LogsModule,
    ApiKeyModule,
    LoggerModule,
    BotStateModule,
    BotQueryModule,
    CommandBusModule,
  ],
  controllers: [McpController],
  providers: [McpService],
  exports: [McpService],
})
export class McpModule {}
