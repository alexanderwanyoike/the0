import {
  validateCustomBotConfigPayload,
  customBotConfigSchema,
} from "../custom-bot.schema";
import { CustomBotConfig } from "../custom-bot.types";

describe("Custom Bot Config Schema Validation", () => {
  const validConfig: CustomBotConfig = {
    name: "test-bot",
    version: "1.0.0",
    runtime: "python3.11",
    type: "scheduled",
    author: "Test Author",
    entrypoints: {
      bot: "main.py",
    },
    schema: {
      bot: {
        type: "object",
        properties: {
          param1: { type: "string" },
        },
      },
    },
    readme:
      "This is a comprehensive readme that meets the minimum length requirement for testing purposes.",
    description: "A test bot configuration",
    metadata: {
      tags: ["test", "bot"],
    },
  };

  describe("Required Properties Validation", () => {
    it("should validate when all required properties are present", () => {
      const result = validateCustomBotConfigPayload(validConfig);
      expect(result.valid).toBe(true);
      expect(result.errors).toBeUndefined();
    });

    it("should fail when required properties are missing", () => {
      const invalidConfig = { name: "test-bot" };
      const result = validateCustomBotConfigPayload(invalidConfig);
      expect(result.valid).toBe(false);
      expect(result.errors).toBeDefined();
      expect(result.errors?.some((err) => err.includes("version"))).toBe(true);
    });
  });

  describe("Schema Properties Validation", () => {
    it("should validate bot schema when provided", () => {
      const configWithValidBotSchema = {
        ...validConfig,
        schema: {
          bot: {
            type: "object",
            properties: {
              symbol: { type: "string" },
              quantity: { type: "number" },
            },
            required: ["symbol"],
          },
        },
      };
      const result = validateCustomBotConfigPayload(configWithValidBotSchema);
      expect(result.valid).toBe(true);
    });

    it("should fail when bot schema is invalid JSON schema", () => {
      const configWithInvalidBotSchema = {
        ...validConfig,
        schema: {
          bot: {
            type: "invalid-type",
            properties: "not-an-object",
          },
        },
      };
      const result = validateCustomBotConfigPayload(configWithInvalidBotSchema);
      expect(result.valid).toBe(false);
      expect(result.errors).toBeDefined();
    });

    it("should fail when backtest schema is invalid JSON schema", () => {
      const configWithInvalidBacktestSchema = {
        ...validConfig,
        entrypoints: {
          bot: "main.py",
          backtest: "backtest.py",
        },
        schema: {
          bot: {
            type: "object",
            properties: { param1: { type: "string" } },
          },
          backtest: {
            type: "invalid-type",
            properties: "not-an-object",
          },
        },
      };
      const result = validateCustomBotConfigPayload(
        configWithInvalidBacktestSchema,
      );
      expect(result.valid).toBe(false);
      expect(result.errors).toBeDefined();
    });

    it("should validate backtest schema when provided", () => {
      const configWithValidBacktestSchema = {
        ...validConfig,
        entrypoints: {
          bot: "main.py",
          backtest: "backtest.py",
        },
        schema: {
          bot: {
            type: "object",
            properties: { param1: { type: "string" } },
          },
          backtest: {
            type: "object",
            properties: {
              startDate: { type: "string", format: "date" },
              endDate: { type: "string", format: "date" },
            },
            required: ["startDate", "endDate"],
          },
        },
      };
      const result = validateCustomBotConfigPayload(
        configWithValidBacktestSchema,
      );
      expect(result.valid).toBe(true);
    });

    it("should fail when backtest schema is invalid JSON schema", () => {
      const configWithInvalidBacktestSchema = {
        ...validConfig,
        entrypoints: {
          bot: "main.py",
          backtest: "backtest.py",
        },
        schema: {
          bot: {
            type: "object",
            invalidKeyword: "invalid-value",
            properties: { param1: { type: "string" } },
          },
          backtest: {
            type: "invalid-type",
            properties: "not-an-object",
          },
        },
      };
      const result = validateCustomBotConfigPayload(
        configWithInvalidBacktestSchema,
      );
      expect(result.valid).toBe(false);
      expect(result.errors).toBeDefined();
    });
  });

  describe("Property Format Validation", () => {
    it("should validate name pattern", () => {
      const invalidNameConfig = { ...validConfig, name: "Invalid_Name!" };
      const result = validateCustomBotConfigPayload(invalidNameConfig);
      expect(result.valid).toBe(false);
      expect(result.errors?.some((err) => err.includes("pattern"))).toBe(true);
    });

    it("should validate version pattern", () => {
      const invalidVersionConfig = { ...validConfig, version: "1.0" };
      const result = validateCustomBotConfigPayload(invalidVersionConfig);
      expect(result.valid).toBe(false);
      expect(result.errors?.some((err) => err.includes("pattern"))).toBe(true);
    });
  });

  describe("Conditional Schema Validation", () => {
    it("should enforce python3.11 runtime for scheduled type", () => {
      const invalidScheduledConfig = {
        ...validConfig,
        type: "scheduled" as const,
        runtime: "nodejs20" as any,
      };
      const result = validateCustomBotConfigPayload(invalidScheduledConfig);
      expect(result.valid).toBe(false);
    });

    it("should require backtest schema when backtest entrypoint is provided", () => {
      const configWithBacktestEntrypoint = {
        ...validConfig,
        entrypoints: {
          bot: "main.py",
          backtest: "backtest.py",
        },
        schema: {
          bot: {
            type: "object",
            properties: { param1: { type: "string" } },
          },
          // Missing backtest schema
        },
      };
      const result = validateCustomBotConfigPayload(
        configWithBacktestEntrypoint,
      );
      expect(result.valid).toBe(false);
    });
  });

  describe("Edge Cases", () => {
    it("should handle empty config object", () => {
      const result = validateCustomBotConfigPayload({});
      expect(result.valid).toBe(false);
      expect(result.errors).toBeDefined();
    });

    it("should handle null/undefined config", () => {
      const result1 = validateCustomBotConfigPayload(null as any);
      const result2 = validateCustomBotConfigPayload(undefined as any);
      expect(result1.valid).toBe(false);
      expect(result2.valid).toBe(false);
    });

    it("should validate config without schema property", () => {
      const configWithoutSchema: Partial<CustomBotConfig> = {
        name: "test-bot",
        version: "1.0.0",
        runtime: "python3.11",
        type: "scheduled",
        entrypoints: {
          bot: "main.py",
        },
        readme:
          "This is a comprehensive readme that meets the minimum length requirement for testing purposes.",
      };
      const result = validateCustomBotConfigPayload(configWithoutSchema);
      expect(result.valid).toBe(false); // Schema is required
    });
  });
});
