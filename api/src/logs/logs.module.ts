import { Module } from '@nestjs/common';
import { LogsService } from './logs.service';
import { LogsController } from './logs.controller';
import { ConfigModule } from '@nestjs/config';
import { BotModule } from '@/bot/bot.module';
import { ApiKeyModule } from '@/api-key/api-key.module';

@Module({
  imports: [ConfigModule, BotModule, ApiKeyModule],
  controllers: [LogsController],
  providers: [LogsService],
})
export class LogsModule {}
