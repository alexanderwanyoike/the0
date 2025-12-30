import {
  validateCustomBotConfig,
  validateCustomBotConfigPayload,
  validateCustomBotPayload,
} from "../custom-bot.schema";
import { CustomBotConfig } from "../custom-bot.types";

describe("Custom Bot Schema Validation", () => {
  const validConfig: CustomBotConfig = {
    name: "test-bot",
    description: "A valid test bot description",
    version: "1.0.0",
    author: "Test Author",
    type: "scheduled",
    runtime: "python3.11",
    entrypoints: {
      bot: "main.py",
    },
    schema: {
      bot: {
        type: "object",
        properties: {
          apiKey: { type: "string" },
        },
      },
    },
    readme:
      "This is a comprehensive readme that meets the minimum length requirement for bot documentation and provides useful information.",
  };

  describe("validateCustomBotConfig", () => {
    it("should validate a complete valid config", () => {
      const result = validateCustomBotConfig(validConfig);
      expect(result).toBe(true);
    });

    it("should validate config with optional metadata", () => {
      const configWithMetadata = {
        ...validConfig,
        metadata: {
          categories: ["trading", "crypto"],
          instruments: ["bitcoin", "ethereum"],
          exchanges: ["binance", "coinbase"],
          tags: ["test", "demo"],
        },
      };

      const result = validateCustomBotConfig(configWithMetadata);
      expect(result).toBe(true);
    });

    it("should validate with JavaScript entrypoints", () => {
      const jsConfig = {
        ...validConfig,
        entrypoints: {
          bot: "main.js",
        },
      };

      const result = validateCustomBotConfig(jsConfig);
      expect(result).toBe(true);
    });

    it("should validate all bot types", () => {
      const types = ["scheduled", "realtime", "event"] as const;

      types.forEach((type) => {
        const configWithType = { ...validConfig, type };
        const result = validateCustomBotConfig(configWithType);
        expect(result).toBe(true);
      });
    });

    it("should validate realtime bot with nodejs20 runtime", () => {
      const realtimeConfig = {
        ...validConfig,
        type: "realtime" as const,
        runtime: "nodejs20" as const,
      };

      const result = validateCustomBotConfig(realtimeConfig);
      expect(result).toBe(true);
    });

    it("should validate realtime bot with python3.11 runtime", () => {
      const realtimeConfig = {
        ...validConfig,
        type: "realtime" as const,
        runtime: "python3.11" as const,
      };

      const result = validateCustomBotConfig(realtimeConfig);
      expect(result).toBe(true);
    });
  });

  describe("validateCustomBotConfigPayload", () => {
    it("should validate a complete valid config payload", () => {
      const result = validateCustomBotConfigPayload(validConfig);
      expect(result.valid).toBe(true);
      expect(result.errors).toBeUndefined();
    });

    it("should validate payload with optional metadata", () => {
      const configWithMetadata = {
        ...validConfig,
        metadata: {
          categories: ["trading", "crypto"],
          instruments: ["bitcoin", "ethereum"],
          exchanges: ["binance", "coinbase"],
          tags: ["test", "demo"],
          customField: "allowed",
        },
      };

      const result = validateCustomBotConfigPayload(configWithMetadata);
      expect(result.valid).toBe(true);
    });

    it("should fail when name is missing", () => {
      const invalidConfig = { ...validConfig };
      delete (invalidConfig as any).name;

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must have required property 'name'"),
        ),
      ).toBe(true);
    });

    it("should fail when version is missing", () => {
      const invalidConfig = { ...validConfig };
      delete (invalidConfig as any).version;

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must have required property 'version'"),
        ),
      ).toBe(true);
    });

    it("should fail when entrypoints are missing", () => {
      const invalidConfig = { ...validConfig };
      delete (invalidConfig as any).entrypoints;

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must have required property 'entrypoints'"),
        ),
      ).toBe(true);
    });

    it("should fail when schema is missing", () => {
      const invalidConfig = { ...validConfig };
      delete (invalidConfig as any).schema;

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must have required property 'schema'"),
        ),
      ).toBe(true);
    });

    it("should fail when readme is missing", () => {
      const invalidConfig = { ...validConfig };
      delete (invalidConfig as any).readme;

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must have required property 'readme'"),
        ),
      ).toBe(true);
    });

    it("should fail when type is missing", () => {
      const invalidConfig = { ...validConfig };
      delete (invalidConfig as any).type;

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must have required property 'type'"),
        ),
      ).toBe(true);
    });

    it("should fail when runtime is missing", () => {
      const invalidConfig = { ...validConfig };
      delete (invalidConfig as any).runtime;

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must have required property 'runtime'"),
        ),
      ).toBe(true);
    });
  });

  describe("validateCustomBotPayload (backward compatibility)", () => {
    it("should validate config directly", () => {
      const result = validateCustomBotPayload(validConfig);
      expect(result.valid).toBe(true);
    });

    it("should validate payload with config property", () => {
      const payload = { config: validConfig };
      const result = validateCustomBotPayload(payload);
      expect(result.valid).toBe(true);
    });

    it("should handle invalid payload gracefully", () => {
      const result = validateCustomBotPayload(null);
      expect(result.valid).toBe(false);
    });
  });

  describe("runtime validation rules", () => {
    it("should validate scheduled bot with python3.11 runtime", () => {
      const scheduledConfig = {
        ...validConfig,
        type: "scheduled" as const,
        runtime: "python3.11" as const,
      };

      const result = validateCustomBotConfigPayload(scheduledConfig);
      expect(result.valid).toBe(true);
    });

    it("should succeed scheduled bot with nodejs20 runtime", () => {
      // All runtimes are now valid for scheduled bots
      const scheduledConfig = {
        ...validConfig,
        type: "scheduled" as const,
        runtime: "nodejs20" as const,
      };

      const result = validateCustomBotConfigPayload(scheduledConfig);
      expect(result.valid).toBe(true);
    });

    it("should validate realtime bot with python3.11 runtime", () => {
      const realtimeConfig = {
        ...validConfig,
        type: "realtime" as const,
        runtime: "python3.11" as const,
      };

      const result = validateCustomBotConfigPayload(realtimeConfig);
      expect(result.valid).toBe(true);
    });

    it("should validate realtime bot with nodejs20 runtime", () => {
      const realtimeConfig = {
        ...validConfig,
        type: "realtime" as const,
        runtime: "nodejs20" as const,
      };

      const result = validateCustomBotConfigPayload(realtimeConfig);
      expect(result.valid).toBe(true);
    });

    it("should validate event bot with either runtime", () => {
      const eventConfigPython = {
        ...validConfig,
        type: "event" as const,
        runtime: "python3.11" as const,
      };

      const eventConfigNode = {
        ...validConfig,
        type: "event" as const,
        runtime: "nodejs20" as const,
      };

      expect(validateCustomBotConfigPayload(eventConfigPython).valid).toBe(
        true,
      );
      expect(validateCustomBotConfigPayload(eventConfigNode).valid).toBe(true);
    });
  });

  describe("invalid configs - format validation", () => {
    it("should fail with invalid bot name format", () => {
      const invalidConfig = {
        ...validConfig,
        name: "Invalid_Name_With_Underscores!",
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) => err.includes("must match pattern")),
      ).toBe(true);
    });

    it("should fail with invalid version format", () => {
      const invalidConfig = {
        ...validConfig,
        version: "1.0", // Missing patch version
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) => err.includes("must match pattern")),
      ).toBe(true);
    });

    it("should accept any entrypoint file extension", () => {
      // Schema no longer enforces specific file extensions to support compiled languages
      const configWithAnyExtension = {
        ...validConfig,
        entrypoints: {
          bot: "main.txt", // Any extension is now valid
        },
      };

      const result = validateCustomBotConfigPayload(configWithAnyExtension);
      expect(result.valid).toBe(true);
    });

    it("should fail with invalid bot type", () => {
      const invalidConfig = {
        ...validConfig,
        type: "invalid-type" as any,
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must be equal to one of the allowed values"),
        ),
      ).toBe(true);
    });
  });

  describe("invalid configs - length validation", () => {
    it("should fail with name too short", () => {
      const invalidConfig = {
        ...validConfig,
        name: "ab", // Too short
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must NOT have fewer than 3 characters"),
        ),
      ).toBe(true);
    });

    it("should fail with name too long", () => {
      const invalidConfig = {
        ...validConfig,
        name: "a".repeat(51), // Too long
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must NOT have more than 50 characters"),
        ),
      ).toBe(true);
    });

    it("should fail with readme too short", () => {
      const invalidConfig = {
        ...validConfig,
        readme: "Too short", // Less than 50 characters
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must NOT have fewer than 50 characters"),
        ),
      ).toBe(true);
    });

    it("should fail with readme too long", () => {
      const invalidConfig = {
        ...validConfig,
        readme: "a".repeat(10001), // Too long
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must NOT have more than 10000 characters"),
        ),
      ).toBe(true);
    });

    it("should fail with description too long", () => {
      const invalidConfig = {
        ...validConfig,
        description: "a".repeat(501), // Too long
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must NOT have more than 500 characters"),
        ),
      ).toBe(true);
    });

    it("should fail with author too long", () => {
      const invalidConfig = {
        ...validConfig,
        author: "a".repeat(101), // Too long
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must NOT have more than 100 characters"),
        ),
      ).toBe(true);
    });
  });

  describe("invalid configs - array limits", () => {
    it("should fail with too many categories", () => {
      const invalidConfig = {
        ...validConfig,
        metadata: {
          categories: Array(11).fill("category"), // Too many
        },
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must NOT have more than 10 items"),
        ),
      ).toBe(true);
    });

    it("should fail with too many tags", () => {
      const invalidConfig = {
        ...validConfig,
        metadata: {
          tags: Array(21).fill("tag"), // Too many
        },
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must NOT have more than 20 items"),
        ),
      ).toBe(true);
    });

    it("should fail with too many instruments", () => {
      const invalidConfig = {
        ...validConfig,
        metadata: {
          instruments: Array(21).fill("instrument"), // Too many
        },
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must NOT have more than 20 items"),
        ),
      ).toBe(true);
    });

    it("should fail with too many exchanges", () => {
      const invalidConfig = {
        ...validConfig,
        metadata: {
          exchanges: Array(21).fill("exchange"), // Too many
        },
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must NOT have more than 20 items"),
        ),
      ).toBe(true);
    });
  });

  describe("schema validation edge cases", () => {
    it("should fail with additional properties in config root", () => {
      const invalidConfig = {
        ...validConfig,
        extraProperty: "not allowed",
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must NOT have additional properties"),
        ),
      ).toBe(true);
    });

    it("should fail with additional properties in entrypoints", () => {
      const invalidConfig = {
        ...validConfig,
        entrypoints: {
          bot: "main.py",
          extraEntry: "extra.py", // Not allowed
        },
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must NOT have additional properties"),
        ),
      ).toBe(true);
    });

    it("should fail with additional properties in schema", () => {
      const invalidConfig = {
        ...validConfig,
        schema: {
          bot: {},
          extraSchema: {}, // Not allowed
        },
      };

      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(
        result.errors?.some((err) =>
          err.includes("must NOT have additional properties"),
        ),
      ).toBe(true);
    });

    it("should allow additional properties in metadata", () => {
      const configWithExtraMetadata = {
        ...validConfig,
        metadata: {
          categories: ["trading"],
          customField: "allowed",
          anotherField: { nested: "also allowed" },
        },
      };

      const result = validateCustomBotConfigPayload(configWithExtraMetadata);
      expect(result.valid).toBe(true);
    });

    it("should validate missing optional fields", () => {
      const minimalConfig: Partial<CustomBotConfig> = {
        name: "minimal-bot",
        version: "1.0.0",
        type: "scheduled",
        runtime: "python3.11",
        entrypoints: {
          bot: "main.py",
        },
        schema: {
          bot: {
            type: "object",
            properties: { param1: { type: "string" } },
          },
        },
        readme:
          "This is a minimal but valid readme that meets the length requirement for validation.",
      };

      const result = validateCustomBotConfigPayload(minimalConfig);
      expect(result.valid).toBe(true);
    });

    it("should validate with empty schemas", () => {
      const configWithEmptySchemas = {
        ...validConfig,
        schema: {
          bot: {},
        },
      };

      const result = validateCustomBotConfigPayload(configWithEmptySchemas);
      expect(result.valid).toBe(true);
    });

    it("should validate with complex schemas", () => {
      const configWithComplexSchemas = {
        ...validConfig,
        schema: {
          bot: {
            type: "object",
            properties: {
              apiKey: { type: "string", minLength: 1 },
              exchanges: {
                type: "array",
                items: { type: "string" },
                minItems: 1,
              },
              riskSettings: {
                type: "object",
                properties: {
                  maxLoss: { type: "number", minimum: 0 },
                  stopLoss: { type: "boolean" },
                },
              },
            },
            required: ["apiKey"],
          },
        },
      };

      const result = validateCustomBotConfigPayload(configWithComplexSchemas);
      expect(result.valid).toBe(true);
    });
  });

  describe("real-world validation scenarios", () => {
    it("should validate a realistic trading bot config", () => {
      const tradingBotConfig = {
        name: "arbitrage-scanner",
        description:
          "Multi-exchange arbitrage opportunity scanner with automated execution",
        version: "2.1.3",
        author: "Trading Team",
        type: "realtime" as const,
        runtime: "python3.11" as const,
        entrypoints: {
          bot: "src/arbitrage_bot.py",
        },
        schema: {
          bot: {
            type: "object",
            properties: {
              exchanges: { type: "array", items: { type: "string" } },
              minProfitThreshold: { type: "number", minimum: 0.001 },
              maxTradeAmount: { type: "number", minimum: 0 },
              apiCredentials: {
                type: "object",
                properties: {
                  binance: { type: "object" },
                  coinbase: { type: "object" },
                },
              },
            },
            required: ["exchanges", "minProfitThreshold"],
          },
        },
        readme: `# Arbitrage Scanner Bot

This bot scans multiple cryptocurrency exchanges for arbitrage opportunities and can automatically execute trades when profitable spreads are detected.

## Features
- Real-time price monitoring across multiple exchanges
- Automated arbitrage detection and execution
- Risk management with configurable thresholds

## Configuration
The bot requires API credentials for supported exchanges and allows customization of profit thresholds and trade limits.`,
        metadata: {
          categories: ["arbitrage", "multi-exchange"],
          instruments: ["BTC", "ETH", "USDT"],
          exchanges: ["binance", "coinbase", "kraken"],
          tags: ["automated", "real-time", "profitable"],
          license: "MIT",
          minimumBalance: 1000,
        },
      };

      const result = validateCustomBotConfigPayload(tradingBotConfig);
      expect(result.valid).toBe(true);
    });

    it("should validate a scheduled analytics bot config", () => {
      const analyticsBotConfig = {
        name: "market-analytics",
        description: "Daily market analysis and reporting bot",
        version: "1.0.0",
        author: "Analytics Team",
        type: "scheduled" as const,
        runtime: "python3.11" as const,
        entrypoints: {
          bot: "analytics/daily_report.py",
        },
        schema: {
          bot: {
            type: "object",
            properties: {
              reportEmail: { type: "string", format: "email" },
              markets: { type: "array", items: { type: "string" } },
              analysisDepth: {
                type: "string",
                enum: ["basic", "detailed", "comprehensive"],
              },
            },
          },
        },
        readme:
          "Market analytics bot that generates daily reports on cryptocurrency market trends and provides insights for trading decisions. Supports email delivery and customizable analysis depth.",
        metadata: {
          categories: ["analytics", "reporting"],
          tags: ["daily", "email", "trends"],
        },
      };

      const result = validateCustomBotConfigPayload(analyticsBotConfig);
      expect(result.valid).toBe(true);
    });

    it("should validate a notification bot config", () => {
      const notificationBotConfig = {
        name: "notification-bot",
        description: "Real-time price alert and notification bot",
        version: "1.0.0",
        author: "Notification Team",
        type: "realtime" as const,
        runtime: "nodejs20" as const,
        entrypoints: {
          bot: "src/notification_bot.js",
        },
        schema: {
          bot: {
            type: "object",
            properties: {
              webhookUrl: { type: "string", format: "uri" },
              priceThresholds: {
                type: "object",
                properties: {
                  BTC: { type: "number", minimum: 0 },
                  ETH: { type: "number", minimum: 0 },
                },
              },
              notificationChannels: {
                type: "array",
                items: { type: "string", enum: ["email", "slack", "webhook"] },
              },
            },
            required: ["webhookUrl"],
          },
        },
        readme: `# Notification Bot

This bot monitors cryptocurrency prices and sends notifications when certain thresholds are reached.

## Features
- Real-time price monitoring
- Multiple notification channels
- Customizable price thresholds
- Webhook integration`,
        metadata: {
          categories: ["notification", "alerts"],
          tags: ["real-time", "webhook", "alerts"],
        },
      };

      const result = validateCustomBotConfigPayload(notificationBotConfig);
      expect(result.valid).toBe(true);
    });
  });
});
