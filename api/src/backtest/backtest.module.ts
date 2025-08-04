import { Module, forwardRef } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { BacktestService } from './backtest.service';
import { BacktestController } from './backtest.controller';
import { BacktestRepository } from './backtest.repository';
import { BacktestValidator } from './backtest.validator';
import { BacktestEventsService } from './backtest-events.service';
import { CustomBotModule } from '@/custom-bot/custom-bot.module';
import { ApiKeyModule } from '@/api-key/api-key.module';
import { NatsModule } from '@/nats/nats.module';
@Module({
  imports: [
    ConfigModule,
    ApiKeyModule,
    CustomBotModule,
    NatsModule,
  ],
  controllers: [BacktestController],
  providers: [BacktestService, BacktestRepository, BacktestValidator, BacktestEventsService],
})
export class BacktestModule {}
