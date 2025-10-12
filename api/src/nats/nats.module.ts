import { Module } from "@nestjs/common";
import { ConfigModule } from "@nestjs/config";
import { NatsService } from "./nats.service";

@Module({
  imports: [ConfigModule],
  providers: [NatsService],
  exports: [NatsService],
})
export class NatsModule {}
