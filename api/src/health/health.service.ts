import { Inject, Injectable } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { PinoLogger } from "nestjs-pino";
import { NatsService } from "@/nats/nats.service";
import { MINIO_CLIENT } from "@/minio/minio.provider";
import { getDatabase } from "@/database/connection";
import { sql } from "drizzle-orm";
import { errorMessage } from "@/common/result";
import * as Minio from "minio";

export interface HealthStatus {
  status: "ok" | "degraded" | "unhealthy";
  checks: {
    postgres: ComponentHealth;
    nats: ComponentHealth;
    minio: ComponentHealth;
  };
}

interface ComponentHealth {
  status: "up" | "down";
  latencyMs?: number;
  error?: string;
}

@Injectable()
export class HealthService {
  private readonly minioBucket: string;

  constructor(
    private readonly natsService: NatsService,
    @Inject(MINIO_CLIENT) private readonly minioClient: Minio.Client,
    private readonly configService: ConfigService,
    private readonly logger: PinoLogger,
  ) {
    this.minioBucket =
      this.configService.get<string>("CUSTOM_BOTS_BUCKET") || "custom-bots";
  }

  async getLiveness(): Promise<{ status: "ok" }> {
    return { status: "ok" };
  }

  async getReadiness(): Promise<HealthStatus> {
    const [postgres, nats, minio] = await Promise.all([
      this.checkPostgres(),
      this.checkNats(),
      this.checkMinio(),
    ]);

    const checks = { postgres, nats, minio };
    const allUp = Object.values(checks).every((c) => c.status === "up");
    const allDown = Object.values(checks).every((c) => c.status === "down");

    return {
      status: allUp ? "ok" : allDown ? "unhealthy" : "degraded",
      checks,
    };
  }

  private async checkPostgres(): Promise<ComponentHealth> {
    const start = Date.now();
    try {
      const db = getDatabase();
      await db.execute(sql`SELECT 1`);
      return { status: "up", latencyMs: Date.now() - start };
    } catch (error: unknown) {
      this.logger.warn({ err: error }, "Postgres health check failed");
      return { status: "down", error: errorMessage(error) };
    }
  }

  private async checkNats(): Promise<ComponentHealth> {
    const start = Date.now();
    try {
      const connected = await this.natsService.isConnected();
      if (!connected) {
        return { status: "down", error: "Not connected" };
      }
      return { status: "up", latencyMs: Date.now() - start };
    } catch (error: unknown) {
      this.logger.warn({ err: error }, "NATS health check failed");
      return { status: "down", error: errorMessage(error) };
    }
  }

  private async checkMinio(): Promise<ComponentHealth> {
    const start = Date.now();
    try {
      await this.minioClient.bucketExists(this.minioBucket);
      return { status: "up", latencyMs: Date.now() - start };
    } catch (error: unknown) {
      this.logger.warn({ err: error }, "MinIO health check failed");
      return { status: "down", error: errorMessage(error) };
    }
  }
}
