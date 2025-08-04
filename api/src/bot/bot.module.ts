import { Module, forwardRef } from '@nestjs/common';
import { BotService } from './bot.service';
import { BotController } from './bot.controller';
import { BotRepository } from './bot.repository';
import { HttpModule } from '@nestjs/axios';
import { BotValidator } from './bot.validator';
import { ConfigModule } from '@nestjs/config';
import { CustomBotModule } from '@/custom-bot/custom-bot.module';
import { ApiKeyModule } from '@/api-key/api-key.module';
import { UserBotsModule } from '@/user-bots/user-bots.module';
import { NatsModule } from '@/nats/nats.module';
// FeatureGateModule removed for OSS version

@Module({
  imports: [
    HttpModule,
    ConfigModule,
    CustomBotModule,
    ApiKeyModule,
    UserBotsModule,
    NatsModule,
  ],
  controllers: [BotController],
  providers: [BotService, BotRepository, BotValidator],
  exports: [BotService, BotRepository, BotValidator],
})
export class BotModule {}
