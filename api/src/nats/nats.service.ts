import { Injectable, OnModuleDestroy, OnModuleInit } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import {
  connect,
  NatsConnection,
  StringCodec,
  JetStreamManager,
  RetentionPolicy,
  StorageType,
} from "nats";
import { Result, Ok, Failure } from "@/common/result";

@Injectable()
export class NatsService implements OnModuleInit, OnModuleDestroy {
  private connection: NatsConnection | null = null;
  private jetStreamManager: JetStreamManager | null = null;
  private readonly sc = StringCodec();

  constructor(private readonly configService: ConfigService) {}

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

      console.log("✅ Connected to NATS server");
    } catch (error) {
      console.error("❌ Failed to connect to NATS:", error);
      throw error;
    }
  }

  private async disconnect(): Promise<void> {
    if (this.connection) {
      await this.connection.close();
      this.connection = null;
      this.jetStreamManager = null;
      console.log("🔌 Disconnected from NATS server");
    }
  }

  private async setupStreams(): Promise<void> {
    if (!this.jetStreamManager) {
      throw new Error("NATS JetStream manager not initialized");
    }

    try {
      // Try to delete existing stream first to recreate with new configuration
      try {
        await this.jetStreamManager.streams.delete("THE0_EVENTS");
        console.log("🗑️ Deleted existing THE0_EVENTS stream");
      } catch (deleteError: any) {
        if (!deleteError.message?.includes("stream not found")) {
          console.warn(
            "⚠️ Failed to delete existing stream:",
            deleteError.message,
          );
        }
      }

      // Create stream for specific event patterns (avoid wildcards that require no-ack)
      await this.jetStreamManager.streams.add({
        name: "THE0_EVENTS",
        subjects: [
          "custom-bot.submitted",
          "custom-bot.approved",
          "custom-bot.declined",
          "custom-bot.awaiting-human-review",
          "custom-bot.analysis-failed",
          "the0.bot.created",
          "the0.bot.updated",
          "the0.bot.deleted",
          "the0.bot-schedule.created",
          "the0.bot-schedule.updated",
          "the0.bot-schedule.deleted",
          "the0.backtest.created",
          "the0.backtest.completed",
        ],
        retention: RetentionPolicy.Limits,
        max_age: 7 * 24 * 60 * 60 * 1000 * 1000000, // 7 days in nanoseconds
        storage: StorageType.File,
      });

      console.log("📡 NATS JetStream streams initialized");
    } catch (error: any) {
      console.error("❌ Failed to setup NATS streams:", error);
      throw error;
    }
  }

  // Generic publish method - business logic handled by callers
  async publish(topic: string, payload: any): Promise<Result<void, string>> {
    if (!this.connection) {
      return Failure("NATS connection not established");
    }

    try {
      // For backtest events, use regular NATS publishing (not JetStream)
      if (topic.startsWith("the0.backtest.")) {
        this.connection.publish(topic, this.sc.encode(JSON.stringify(payload)));
        console.log(`✅ Published to NATS topic: ${topic}`);
        return Ok(undefined);
      }

      // For other events, use JetStream
      const jetStream = this.connection.jetstream();
      await jetStream.publish(topic, this.sc.encode(JSON.stringify(payload)), {
        timeout: 5000, // 5 second timeout
      });
      return Ok(undefined);
    } catch (error: any) {
      console.error(`Failed to publish to topic ${topic}:`, error);
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
