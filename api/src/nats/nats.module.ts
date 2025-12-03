import { Module } from "@nestjs/common";
import { ConfigModule } from "@nestjs/config";
import { NatsService } from "./nats.service";
import { LoggerModule } from "@/logger/logger.module";

@Module({
  imports: [ConfigModule, LoggerModule],
  providers: [NatsService],
  exports: [NatsService],
})
export class NatsModule {}
