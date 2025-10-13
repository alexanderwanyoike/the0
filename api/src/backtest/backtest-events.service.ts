import { Injectable, OnModuleInit, OnModuleDestroy } from "@nestjs/common";
import { NatsService } from "@/nats/nats.service";
import { BacktestRepository } from "./backtest.repository";
import { StringCodec } from "nats";

interface BacktestCompletionEvent {
  backtest_id: string;
  success: boolean;
  error?: string;
  timestamp: string;
}

@Injectable()
export class BacktestEventsService implements OnModuleInit, OnModuleDestroy {
  private readonly sc = StringCodec();
  private subscriptions: any[] = [];

  constructor(
    private readonly natsService: NatsService,
    private readonly backtestRepository: BacktestRepository,
  ) {}

  async onModuleInit() {
    await this.setupEventListeners();
  }

  async onModuleDestroy() {
    // Clean up subscriptions
    for (const sub of this.subscriptions) {
      try {
        await sub.unsubscribe();
      } catch (error) {
        console.error("Error unsubscribing from backtest events:", error);
      }
    }
  }

  private async setupEventListeners() {
    try {
      // Wait for NATS connection to be established
      let retries = 0;
      const maxRetries = 10;

      while (retries < maxRetries && !(await this.natsService.isConnected())) {
        console.log(
          `⏳ Waiting for NATS connection for backtest events... (${retries + 1}/${maxRetries})`,
        );
        await new Promise((resolve) => setTimeout(resolve, 1000));
        retries++;
      }

      if (!(await this.natsService.isConnected())) {
        console.error(
          "❌ Failed to establish NATS connection for backtest event listeners",
        );
        return;
      }

      console.log("📡 Setting up backtest completion event listeners...");

      // Listen for backtest completion events (using regular NATS, not JetStream)
      await this.subscribeToCompletionEvents();

      console.log("✅ Backtest event listeners setup complete");
    } catch (error) {
      console.error("❌ Failed to setup backtest event listeners:", error);
    }
  }

  private async subscribeToCompletionEvents() {
    const connection = (this.natsService as any).connection;
    if (!connection) {
      throw new Error("NATS connection not available");
    }

    // Subscribe to backtest completion events using regular NATS (not JetStream)
    const subscription = connection.subscribe("the0.backtest.completed", {
      queue: "backtest-api-handlers",
    });

    this.subscriptions.push(subscription);

    // Handle completion events
    (async () => {
      for await (const msg of subscription) {
        try {
          const eventData: BacktestCompletionEvent = JSON.parse(
            this.sc.decode(msg.data),
          );
          await this.handleCompletionEvent(eventData);
        } catch (error) {
          console.error("Error processing backtest completion event:", error);
        }
      }
    })();

    console.log(
      "📨 Subscribed to backtest completion events (the0.backtest.completed)",
    );
  }

  private async handleCompletionEvent(event: BacktestCompletionEvent) {
    try {
      console.log(
        `📥 Received backtest completion event for ${event.backtest_id}, success: ${event.success}`,
      );

      // Determine the final status
      const status = event.success ? "completed" : "failed";

      // Update the backtest status in PostgreSQL
      const updateResult = await this.backtestRepository.updateStatus(
        event.backtest_id,
        status,
      );

      if (!updateResult.success) {
        console.error(
          `❌ Failed to update backtest ${event.backtest_id} status:`,
          updateResult.error,
        );
        return;
      }

      console.log(
        `✅ Updated backtest ${event.backtest_id} status to ${status}`,
      );

      // Log completion details
      if (event.error) {
        console.log(
          `❌ Backtest ${event.backtest_id} failed with error: ${event.error}`,
        );
      } else {
        console.log(`🎉 Backtest ${event.backtest_id} completed successfully`);
        console.log(
          `📁 Logs and analysis should be available at: backtests/${event.backtest_id}/logs.txt and backtests/${event.backtest_id}/analysis.json`,
        );
      }
    } catch (error) {
      console.error(
        `❌ Error handling completion event for backtest ${event.backtest_id}:`,
        error,
      );
      throw error;
    }
  }
}
