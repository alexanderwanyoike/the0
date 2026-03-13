import { Module } from "@nestjs/common";
import { NatsModule } from "@/nats/nats.module";
import { LoggerModule } from "@/logger/logger.module";
import { HealthController } from "./health.controller";
import { HealthService } from "./health.service";

@Module({
  imports: [NatsModule, LoggerModule],
  controllers: [HealthController],
  providers: [HealthService],
})
export class HealthModule {}
