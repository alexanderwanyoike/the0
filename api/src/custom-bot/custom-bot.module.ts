import { Module } from "@nestjs/common";
import { CustomBotController } from "./custom-bot.controller";
import { CustomBotService } from "./custom-bot.service";
import { CustomBotRepository } from "./custom-bot.repository";
import { StorageService } from "./storage.service";
import { ApiKeyModule } from "@/api-key/api-key.module";
import { ConfigModule } from "@nestjs/config";

@Module({
  imports: [ConfigModule, ApiKeyModule],
  controllers: [CustomBotController],
  providers: [CustomBotService, CustomBotRepository, StorageService],
  exports: [CustomBotService, CustomBotRepository],
})
export class CustomBotModule {}
