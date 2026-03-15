import { Controller, Get, HttpCode, HttpStatus, Res } from "@nestjs/common";
import { Response } from "express";
import { HealthService } from "./health.service";

@Controller("health")
export class HealthController {
  constructor(private readonly healthService: HealthService) {}

  @Get()
  @HttpCode(HttpStatus.OK)
  async liveness() {
    return this.healthService.getLiveness();
  }

  @Get("ready")
  async readiness(@Res() res: Response) {
    const health = await this.healthService.getReadiness();
    const status =
      health.status === "ok"
        ? HttpStatus.OK
        : HttpStatus.SERVICE_UNAVAILABLE;
    res.status(status).json(health);
  }
}
