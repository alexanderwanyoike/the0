import { Injectable, OnModuleInit, OnModuleDestroy } from "@nestjs/common";
import { NatsService } from "@/nats/nats.service";
import { CustomBotRepository } from "./custom-bot.repository";
import { StringCodec, consumerOpts } from "nats";

interface CustomBotAnalysisEvent {
  type: string;
  botId: string;
  status?: string;
  analysis?: any;
  timestamp: string;
  service: string;
  reasons?: string[];
  score?: number;
  error?: string;
  details?: any;
}

@Injectable()
export class CustomBotEventsService implements OnModuleInit, OnModuleDestroy {
  private readonly sc = StringCodec();
  private subscriptions: any[] = [];

  constructor(
    private readonly natsService: NatsService,
    private readonly customBotRepository: CustomBotRepository,
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
        console.error("Error unsubscribing from NATS:", error);
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
          `⏳ Waiting for NATS connection... (${retries + 1}/${maxRetries})`,
        );
        await new Promise((resolve) => setTimeout(resolve, 1000));
        retries++;
      }

      if (!(await this.natsService.isConnected())) {
        console.error(
          "❌ Failed to establish NATS connection for event listeners",
        );
        return;
      }

      console.log("📡 Setting up custom bot event listeners...");

      // Listen for analysis completion events
      await this.subscribeToAnalysisEvents();

      console.log("✅ Custom bot event listeners setup complete");
    } catch (error) {
      console.error("❌ Failed to setup custom bot event listeners:", error);
    }
  }

  private async subscribeToAnalysisEvents() {
    const connection = (this.natsService as any).connection;
    if (!connection) {
      throw new Error("NATS connection not available");
    }

    const jetStream = connection.jetstream();

    // Subscribe to all custom-bot analysis events
    const opts = consumerOpts();
    opts.deliverTo("custom-bot-approved-deliver");
    opts.manualAck();
    opts.queue("custom-bot-api-handlers");
    const subscription = await jetStream.subscribe("custom-bot.approved", opts);

    this.subscriptions.push(subscription);

    // Handle approved events
    (async () => {
      for await (const msg of subscription) {
        try {
          const eventData: CustomBotAnalysisEvent = JSON.parse(
            this.sc.decode(msg.data),
          );
          await this.handleAnalysisEvent(eventData);
          msg.ack();
        } catch (error) {
          console.error("Error processing custom-bot.approved event:", error);
          msg.nak();
        }
      }
    })();

    // Subscribe to declined events
    const declinedOpts = consumerOpts();
    declinedOpts.deliverTo("custom-bot-declined-deliver");
    declinedOpts.manualAck();
    declinedOpts.queue("custom-bot-api-handlers");
    const declinedSub = await jetStream.subscribe(
      "custom-bot.declined",
      declinedOpts,
    );

    this.subscriptions.push(declinedSub);

    (async () => {
      for await (const msg of declinedSub) {
        try {
          const eventData: CustomBotAnalysisEvent = JSON.parse(
            this.sc.decode(msg.data),
          );
          await this.handleAnalysisEvent(eventData);
          msg.ack();
        } catch (error) {
          console.error("Error processing custom-bot.declined event:", error);
          msg.nak();
        }
      }
    })();

    // Subscribe to awaiting review events
    const reviewOpts = consumerOpts();
    reviewOpts.deliverTo("custom-bot-review-deliver");
    reviewOpts.manualAck();
    reviewOpts.queue("custom-bot-api-handlers");
    const reviewSub = await jetStream.subscribe(
      "custom-bot.awaiting-human-review",
      reviewOpts,
    );

    this.subscriptions.push(reviewSub);

    (async () => {
      for await (const msg of reviewSub) {
        try {
          const eventData: CustomBotAnalysisEvent = JSON.parse(
            this.sc.decode(msg.data),
          );
          await this.handleAnalysisEvent(eventData);
          msg.ack();
        } catch (error) {
          console.error(
            "Error processing custom-bot.awaiting-human-review event:",
            error,
          );
          msg.nak();
        }
      }
    })();

    // Subscribe to analysis failed events
    const failedOpts = consumerOpts();
    failedOpts.deliverTo("custom-bot-failed-deliver");
    failedOpts.manualAck();
    failedOpts.queue("custom-bot-api-handlers");
    const failedSub = await jetStream.subscribe(
      "custom-bot.analysis-failed",
      failedOpts,
    );

    this.subscriptions.push(failedSub);

    (async () => {
      for await (const msg of failedSub) {
        try {
          const eventData: CustomBotAnalysisEvent = JSON.parse(
            this.sc.decode(msg.data),
          );
          await this.handleAnalysisFailedEvent(eventData);
          msg.ack();
        } catch (error) {
          console.error(
            "Error processing custom-bot.analysis-failed event:",
            error,
          );
          msg.nak();
        }
      }
    })();

    console.log("📨 Subscribed to custom bot analysis events");
  }

  private async handleAnalysisEvent(event: CustomBotAnalysisEvent) {
    try {
      console.log(`📥 Received ${event.type} event for bot ${event.botId}`);

      // Map event type to status
      let status: string;
      switch (event.type) {
        case "custom-bot.approved":
          status = "approved";
          break;
        case "custom-bot.declined":
          status = "declined";
          break;
        case "custom-bot.awaiting-human-review":
          status = "awaiting_human_review";
          break;
        default:
          console.warn(`⚠️ Unknown event type: ${event.type}`);
          return;
      }

      // Update the custom bot status in the database
      const updateResult = await this.customBotRepository.updateBotStatus(
        event.botId,
        status,
      );

      if (!updateResult.success) {
        console.error(
          `❌ Failed to update bot ${event.botId} status:`,
          updateResult.error,
        );
        return;
      }

      // Store analysis results if available
      if (event.analysis) {
        const analysisResult = await this.customBotRepository.updateBotAnalysis(
          event.botId,
          event.analysis,
        );

        if (!analysisResult.success) {
          console.error(
            `❌ Failed to store analysis for bot ${event.botId}:`,
            analysisResult.error,
          );
        }
      }

      console.log(`✅ Updated bot ${event.botId} status to ${status}`);

      // Log analysis summary
      if (event.analysis) {
        const score = event.analysis.score || event.score || 0;
        const filesScanned = event.analysis.filesScanned?.length || 0;
        const threatLevel =
          event.analysis.threatSummary?.threatLevel || "unknown";

        console.log(
          `📊 Analysis summary for ${event.botId}: Score=${score}/5, Files=${filesScanned}, Threat=${threatLevel}`,
        );
      }
    } catch (error) {
      console.error(
        `❌ Error handling ${event.type} event for bot ${event.botId}:`,
        error,
      );
      throw error; // Re-throw to trigger NATS retry
    }
  }

  private async handleAnalysisFailedEvent(event: CustomBotAnalysisEvent) {
    try {
      console.log(`📥 Received analysis-failed event for bot ${event.botId}`);

      // Set status to awaiting human review for failed analyses
      const updateResult = await this.customBotRepository.updateBotStatus(
        event.botId,
        "awaiting_human_review",
      );

      if (!updateResult.success) {
        console.error(
          `❌ Failed to update failed bot ${event.botId} status:`,
          updateResult.error,
        );
        return;
      }

      // Store error information
      const errorAnalysis = {
        reviewedAt: event.timestamp,
        reviewedBy: event.service || "0vers33r",
        version: "v3.0-oss",
        score: 5, // Max score for errors
        issues: ["analysis_error"],
        error: event.error,
        details: event.details,
        filesScanned: [],
      };

      const analysisResult = await this.customBotRepository.updateBotAnalysis(
        event.botId,
        errorAnalysis,
      );

      if (!analysisResult.success) {
        console.error(
          `❌ Failed to store error analysis for bot ${event.botId}:`,
          analysisResult.error,
        );
      }

      console.log(
        `⚠️ Marked bot ${event.botId} for human review due to analysis failure`,
      );
    } catch (error) {
      console.error(
        `❌ Error handling analysis-failed event for bot ${event.botId}:`,
        error,
      );
      throw error;
    }
  }
}
