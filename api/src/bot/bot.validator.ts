import { Injectable } from "@nestjs/common";
import { Result, Failure, Ok } from "@/common/result";
import Ajv from "ajv";
import addFormats from "ajv-formats";
import { isValidCron } from "cron-validator";
import { CustomBot } from "@/custom-bot/custom-bot.types";
import { BOT_TYPE_PATTERN } from "@/bot/bot.constants";
import { BotConfig } from "@/database/schema/bots";

/** JSON Schema object shape used for bot config validation */
interface JsonSchema {
  type?: string;
  properties?: Record<string, JsonSchema>;
  additionalProperties?: boolean;
  [key: string]: unknown;
}

@Injectable()
export class BotValidator {
  async validate(
    config: BotConfig,
    customBot: CustomBot,
  ): Promise<Result<boolean, string[]>> {
    const { type } = config;

    if (!type) {
      return Failure(["No type provided"]);
    }

    if (!BOT_TYPE_PATTERN.test(type)) {
      return Failure(["Invalid bot type format. Expected format: type/name"]);
    }

    const [botType] = type.split("/");

    if (botType === "scheduled") {
      if (!config.schedule) {
        return Failure(["No schedule provided"]);
      }

      if (!isValidCron(config.schedule)) {
        return Failure([`${config.schedule} is not a valid cron expression`]);
      }
    }
    return this.validateWithSchema(customBot, config);
  }

  private validateWithSchema(
    customBot: CustomBot,
    config: BotConfig,
  ): Result<boolean, string[]> {
    const {
      config: {
        schema: { bot },
      },
    } = customBot;

    // Filter config if additionalProperties is false
    const configToValidate = this.filterConfigForSchema(config, bot);

    const ajv = new Ajv({ allErrors: true, strictSchema: false });
    addFormats(ajv);
    const validate = ajv.compile(bot);
    if (validate(configToValidate)) {
      return Ok(true);
    } else {
      const errors = validate.errors;
      return Failure<boolean, string[]>(
        errors?.map((error) => {
          if (!error.instancePath) {
            return error.message;
          }
          return `${error.instancePath} ${error.message}`;
        }) || [],
      );
    }
  }

  private filterConfigForSchema(
    config: Record<string, unknown>,
    schema: JsonSchema,
  ): Record<string, unknown> {
    // If additionalProperties is not explicitly false, return original config
    if (schema.additionalProperties !== false) {
      return config;
    }

    // If no properties defined in schema, return original config
    if (!schema.properties) {
      return config;
    }

    // Recursively filter config to only include schema properties
    return this.filterObjectProperties(config, schema);
  }

  private filterObjectProperties(
    obj: Record<string, unknown>,
    schema: JsonSchema,
  ): Record<string, unknown> {
    if (!obj || typeof obj !== "object" || Array.isArray(obj)) {
      return obj;
    }

    const filtered: Record<string, unknown> = {};
    const schemaProperties = schema.properties || {};

    for (const key of Object.keys(schemaProperties)) {
      if (obj[key] !== undefined) {
        const propertySchema = schemaProperties[key];

        if (
          propertySchema.type === "object" &&
          propertySchema.properties &&
          propertySchema.additionalProperties === false
        ) {
          filtered[key] = this.filterObjectProperties(
            obj[key] as Record<string, unknown>,
            propertySchema,
          );
        } else {
          filtered[key] = obj[key];
        }
      }
    }

    return filtered;
  }
}
