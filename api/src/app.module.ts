import { Module } from "@nestjs/common";
import { AppController } from "./app.controller";
import { AppService } from "./app.service";
import { BotModule } from "./bot/bot.module";
import { AuthModule } from "./auth/auth.module";
import { ConfigModule } from "@nestjs/config";
import { CustomBotModule } from "./custom-bot/custom-bot.module";
import { ApiKeyModule } from "./api-key/api-key.module";
import { LogsModule } from "./logs/logs.module";
import { NatsModule } from "./nats/nats.module";
import { LoggerModule } from "./logger/logger.module";
import { McpModule } from "./mcp/mcp.module";
import { BotStateModule } from "./bot-state/bot-state.module";
import { MinioModule } from "./minio";
import configuration from "./config/configuration";

@Module({
  imports: [
    ConfigModule.forRoot({
      load: [configuration],
      isGlobal: true,
    }),
    MinioModule,
    LoggerModule,
    BotModule,
    AuthModule,
    CustomBotModule,
    ApiKeyModule,
    LogsModule,
    NatsModule,
    McpModule,
    BotStateModule,
  ],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
