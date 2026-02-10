import { Injectable, OnModuleDestroy, OnModuleInit } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { PinoLogger } from "nestjs-pino";
import {
  connect,
  NatsConnection,
  StringCodec,
  JetStreamManager,
  RetentionPolicy,
  StorageType,
  StreamConfig,
} from "nats";
import { Result, Ok, Failure } from "@/common/result";

@Injectable()
export class NatsService implements OnModuleInit, OnModuleDestroy {
  private connection: NatsConnection | null = null;
  private jetStreamManager: JetStreamManager | null = null;
  private readonly sc = StringCodec();

  constructor(
    private readonly configService: ConfigService,
    private readonly logger: PinoLogger,
  ) {}

  async onModuleInit() {
    await this.connect();
    await this.setupStreams();
  }

  async onModuleDestroy() {
    await this.disconnect();
  }

  private async connect(): Promise<void> {
    try {
      const servers = this.configService
        .get<string>("NATS_URLS")
        ?.split(",") || ["nats://localhost:4222"];

      this.connection = await connect({
        servers,
        reconnect: true,
        maxReconnectAttempts: -1,
        reconnectTimeWait: 1000,
      });

      this.jetStreamManager = await this.connection.jetstreamManager();

      this.logger.info("Connected to NATS server");
    } catch (error) {
      this.logger.error({ err: error }, "Failed to connect to NATS");
      throw error;
    }
  }

  private async disconnect(): Promise<void> {
    if (this.connection) {
      await this.connection.close();
      this.connection = null;
      this.jetStreamManager = null;
      this.logger.info("Disconnected from NATS server");
    }
  }

  private async ensureStream(
    config: Partial<StreamConfig> & Pick<StreamConfig, "name">,
  ): Promise<void> {
    try {
      // Check if stream exists
      await this.jetStreamManager!.streams.info(config.name);
      // Stream exists — update config
      await this.jetStreamManager!.streams.update(config.name, config);
    } catch (err: any) {
      // Stream not found — create it
      const isNotFound =
        err?.api_error?.err_code === 10059 ||
        err?.code === "404" ||
        err?.message?.includes("stream not found");
      if (isNotFound) {
        await this.jetStreamManager!.streams.add(config);
      } else {
        throw err;
      }
    }
  }

  private async setupStreams(): Promise<void> {
    if (!this.jetStreamManager) {
      throw new Error("NATS JetStream manager not initialized");
    }

    try {
      await this.ensureStream({
        name: "THE0_EVENTS",
        subjects: [
          "the0.bot.created",
          "the0.bot.updated",
          "the0.bot.deleted",
          "the0.bot-schedule.created",
          "the0.bot-schedule.updated",
          "the0.bot-schedule.deleted",
        ],
        retention: RetentionPolicy.Limits,
        max_age: 7 * 24 * 60 * 60 * 1000 * 1000000, // 7 days in nanoseconds
        storage: StorageType.File,
      });

      await this.ensureStream({
        name: "THE0_BOT_LOGS",
        subjects: ["the0.bot.logs.>"],
        retention: RetentionPolicy.Limits,
        max_age: 60 * 60 * 1000 * 1000000, // 1 hour in nanoseconds
        max_bytes: 128 * 1024 * 1024, // 128MB
        storage: StorageType.Memory,
      });

      this.logger.info("NATS JetStream initialized");
    } catch (error: any) {
      this.logger.error({ err: error }, "Failed to setup NATS streams");
      throw error;
    }
  }

  // Generic publish method - business logic handled by callers
  async publish(topic: string, payload: any): Promise<Result<void, string>> {
    if (!this.connection) {
      return Failure("NATS connection not established");
    }

    try {
      const jetStream = this.connection.jetstream();
      await jetStream.publish(topic, this.sc.encode(JSON.stringify(payload)), {
        timeout: 5000, // 5 second timeout
      });
      return Ok(undefined);
    } catch (error: any) {
      this.logger.error({ err: error, topic }, "Failed to publish to topic");
      return Failure(error.message);
    }
  }

  // Subscribe to a NATS subject using raw connection (not JetStream)
  subscribe(
    subject: string,
    callback: (msg: string) => void,
  ): Result<() => void, string> {
    if (!this.connection) {
      return Failure("NATS connection not established");
    }

    try {
      const sub = this.connection.subscribe(subject, {
        callback: (err, msg) => {
          if (err) {
            this.logger.error({ err, subject }, "NATS subscription error");
            return;
          }
          try {
            callback(this.sc.decode(msg.data));
          } catch (cbErr) {
            this.logger.error(
              { err: cbErr, subject },
              "Error in subscription callback",
            );
          }
        },
      });

      return Ok(() => sub.unsubscribe());
    } catch (error: any) {
      this.logger.error(
        { err: error, subject },
        "Failed to create NATS subscription",
      );
      return Failure(error.message);
    }
  }

  // Health check
  async isConnected(): Promise<boolean> {
    return this.connection !== null && !this.connection.isClosed();
  }

  async getConnectionInfo(): Promise<Result<any, string>> {
    if (!this.connection) {
      return Failure("NATS connection not established");
    }

    try {
      const info = this.connection.info;
      return Ok({
        server_name: info?.server_name,
        version: info?.version,
        go: info?.go,
        host: info?.host,
        port: info?.port,
        max_payload: info?.max_payload,
        connect_urls: info?.connect_urls,
      });
    } catch (error: any) {
      return Failure(error.message);
    }
  }
}
