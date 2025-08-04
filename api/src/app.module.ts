import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { BotModule } from './bot/bot.module';
import { AuthModule } from './auth/auth.module';
import { BacktestModule } from './backtest/backtest.module';
import { ConfigModule } from '@nestjs/config';
import { CustomBotModule } from './custom-bot/custom-bot.module';
import { ApiKeyModule } from './api-key/api-key.module';
import { UserBotsModule } from './user-bots/user-bots.module';
import { LogsModule } from './logs/logs.module';
import { NatsModule } from './nats/nats.module';
import { UploadModule } from './upload/upload.module';
import configuration from './config/configuration';
// Removed OSS-incompatible modules: StripeConnect, Transactions, Subscriptions, FeatureGates, Usage

@Module({
  imports: [
    ConfigModule.forRoot({
      load: [configuration],
      isGlobal: true,
    }),
    BotModule,
    AuthModule,
    BacktestModule,
    CustomBotModule,
    ApiKeyModule,
    UserBotsModule,
    LogsModule,
    NatsModule,
    UploadModule,
  ],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
