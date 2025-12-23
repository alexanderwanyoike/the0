import Ajv from "ajv";
import addFormats from "ajv-formats";
import { CustomBotConfig } from "./custom-bot.types";

const ajv = new Ajv();
const schemaAjv = new Ajv({ strict: true, strictSchema: false });
addFormats(schemaAjv);

export const customBotConfigSchema = {
  type: "object",
  required: [
    "name",
    "version",
    "entrypoints",
    "schema",
    "readme",
    "type",
    "runtime",
  ],
  properties: {
    name: {
      type: "string",
      pattern: "^[a-z0-9-]+$",
      minLength: 3,
      maxLength: 50,
    },
    description: {
      type: "string",
      maxLength: 500,
    },
    version: {
      type: "string",
      pattern: "^\\d+\\.\\d+\\.\\d+$",
    },
    runtime: {
      type: "string",
      enum: ["python3.11", "nodejs20"],
    },
    author: {
      type: "string",
      maxLength: 100,
    },
    type: {
      type: "string",
      enum: ["scheduled", "realtime", "event"],
      default: "scheduled",
    },
    entrypoints: {
      type: "object",
      required: ["bot"],
      properties: {
        bot: {
          type: "string",
          pattern: "\\.(py|js|ts)$",
        },
      },
      additionalProperties: false,
    },
    schema: {
      type: "object",
      required: ["bot"],
      properties: {
        bot: {
          type: "object",
        },
      },
      additionalProperties: false,
    },
    readme: {
      type: "string",
      minLength: 50,
      maxLength: 10000,
    },
    metadata: {
      type: "object",
      properties: {
        categories: {
          type: "array",
          items: { type: "string" },
          maxItems: 10,
        },
        instruments: {
          type: "array",
          items: { type: "string" },
          maxItems: 20,
        },
        exchanges: {
          type: "array",
          items: { type: "string" },
          maxItems: 20,
        },
        tags: {
          type: "array",
          items: { type: "string" },
          maxItems: 20,
        },
      },
      additionalProperties: true, // Allow additional metadata fields
    },
  },
  additionalProperties: false,
  allOf: [
    {
      if: {
        properties: { type: { const: "scheduled" } },
        required: ["type"],
      },
      then: {
        properties: {
          runtime: { enum: ["python3.11"] },
        },
      },
    },
  ],
};

export const validateCustomBotConfig = ajv.compile(customBotConfigSchema);

export function validateCustomBotConfigPayload(
  config: Partial<CustomBotConfig>,
): {
  valid: boolean;
  errors?: string[];
} {
  const valid = validateCustomBotConfig(config);
  if (!valid) {
    return {
      valid: false,
      errors:
        validateCustomBotConfig.errors?.map(
          (err) => `${err.instancePath} ${err.message}`,
        ) || [],
    };
  }

  if (config.schema.bot) {
    const schemaValidationResult = validateSchema(config.schema.bot);
    if (!schemaValidationResult.valid) {
      return {
        valid: false,
        errors: schemaValidationResult.errors,
      };
    }
  }

  return { valid: true };
}

// Validates the actual custom bots schemas using a separate AJV instance
const validateSchema = (
  schema: object,
): { valid: boolean; errors?: string[] } => {
  try {
    schemaAjv.compile(schema);
    return { valid: true };
  } catch (error: any) {
    return { valid: false, errors: [error.message] };
  }
};

// Keep the old validation function name for compatibility but update it
export function validateCustomBotPayload(payload: any): {
  valid: boolean;
  errors?: string[];
} {
  // For backward compatibility, if payload has config property, validate that
  if (payload && payload.config) {
    return validateCustomBotConfigPayload(payload.config);
  }
  // Otherwise validate the payload directly as config
  return validateCustomBotConfigPayload(payload);
}
