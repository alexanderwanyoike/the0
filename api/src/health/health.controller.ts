import { Controller, Get, HttpCode, HttpStatus, Res } from "@nestjs/common";
import {
  ApiOkResponse,
  ApiOperation,
  ApiServiceUnavailableResponse,
  ApiTags,
} from "@nestjs/swagger";
import { Response } from "express";
import { HealthService } from "./health.service";

@ApiTags("health")
@Controller("health")
export class HealthController {
  constructor(private readonly healthService: HealthService) {}

  @Get()
  @HttpCode(HttpStatus.OK)
  @ApiOperation({ summary: "Liveness probe" })
  async liveness() {
    return this.healthService.getLiveness();
  }

  @Get("ready")
  @ApiOperation({
    summary: "Readiness probe",
    description:
      "Checks NATS and MinIO, plus Postgres when HEALTH_CHECK_DATABASE=true.",
  })
  @ApiOkResponse({ description: "Every checked dependency is up" })
  @ApiServiceUnavailableResponse({
    description: "At least one checked dependency is down",
  })
  async readiness(@Res() res: Response) {
    const health = await this.healthService.getReadiness();
    const status =
      health.status === "ok"
        ? HttpStatus.OK
        : HttpStatus.SERVICE_UNAVAILABLE;
    res.status(status).json(health);
  }
}
