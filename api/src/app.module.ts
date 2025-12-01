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
import configuration from "./config/configuration";

@Module({
  imports: [
    ConfigModule.forRoot({
      load: [configuration],
      isGlobal: true,
    }),
    BotModule,
    AuthModule,
    CustomBotModule,
    ApiKeyModule,
    LogsModule,
    NatsModule,
  ],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
