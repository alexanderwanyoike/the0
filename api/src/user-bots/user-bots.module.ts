import { Module } from '@nestjs/common';
import { UserBotsController } from './user-bots.controller';
import { UserBotsService } from './user-bots.service';
import { UserBotsRepository } from './user-bots.repository';
import { CustomBotModule } from '@/custom-bot/custom-bot.module';
import { ApiKeyModule } from '@/api-key/api-key.module';

@Module({
  imports: [ApiKeyModule, CustomBotModule],
  controllers: [UserBotsController],
  providers: [UserBotsService, UserBotsRepository],
  exports: [UserBotsService, UserBotsRepository],
})
export class UserBotsModule {}
