import { Module } from '@nestjs/common';
import { CustomBotController } from './custom-bot.controller';
import { CustomBotService } from './custom-bot.service';
import { CustomBotRepository } from './custom-bot.repository';
import { StorageService } from './storage.service';
import { CustomBotEventsService } from './custom-bot-events.service';
import { ApiKeyModule } from '@/api-key/api-key.module';
import { NatsModule } from '@/nats/nats.module';
import { ConfigModule } from '@nestjs/config';

@Module({
  imports: [ConfigModule, ApiKeyModule, NatsModule],
  controllers: [CustomBotController],
  providers: [
    CustomBotService,
    CustomBotRepository,
    StorageService,
    CustomBotEventsService,
  ],
  exports: [CustomBotService, CustomBotRepository],
})
export class CustomBotModule {}
