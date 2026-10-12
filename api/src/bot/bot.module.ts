import { Module, forwardRef } from "@nestjs/common";
import { BotService } from "./bot.service";
import { BotController } from "./bot.controller";
import { BotRepository } from "./bot.repository";
import { HttpModule } from "@nestjs/axios";
import { BotValidator } from "./bot.validator";
import { BotEventOutbox } from "./bot-event-outbox";
import { BotEventRelay } from "./bot-event-relay";
import { BotConfigResolver } from "./commands/bot-config.resolver";
import { BotEventFactory } from "./commands/bot-event-factory";
import { DeployBotHandler } from "./commands/deploy-bot.handler";
import { UpdateBotHandler } from "./commands/update-bot.handler";
import { DeleteBotHandler } from "./commands/delete-bot.handler";
import { ConfigModule } from "@nestjs/config";
import { CustomBotModule } from "@/custom-bot/custom-bot.module";
import { ApiKeyModule } from "@/api-key/api-key.module";
// UserBotsModule removed for OSS version
import { NatsModule } from "@/nats/nats.module";
// FeatureGateModule removed for OSS version
import { LoggerModule } from "@/logger/logger.module";
import { CommandBusModule } from "@/command-bus/command-bus.module";

@Module({
  imports: [
    HttpModule,
    ConfigModule,
    CustomBotModule,
    ApiKeyModule,
    NatsModule,
    LoggerModule,
    CommandBusModule,
  ],
  controllers: [BotController],
  providers: [
    BotService,
    BotRepository,
    BotValidator,
    BotEventOutbox,
    BotEventRelay,
    BotConfigResolver,
    BotEventFactory,
    DeployBotHandler,
    UpdateBotHandler,
    DeleteBotHandler,
  ],
  exports: [BotService, BotRepository, BotValidator],
})
export class BotModule {}
