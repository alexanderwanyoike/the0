import { Injectable } from '@nestjs/common';
import { Result, Failure, Ok } from '@/common/result';
import Ajv from 'ajv';
import addFormats from 'ajv-formats';
import { isValidCron } from 'cron-validator';
import { CustomBot } from '@/custom-bot/custom-bot.types';
import { BOT_TYPE_PATTERN } from '@/bot/bot.constants';

@Injectable()
export class BotValidator {
  async validate(
    config: any,
    customBot: CustomBot,
  ): Promise<Result<boolean, string[]>> {
    //Check if the config type is valid
    const { type } = config;

    if (!type) {
      return Failure<boolean, string[]>(['No type provided']);
    }

    if (BOT_TYPE_PATTERN.test(type) === false) {
      return Failure<boolean, string[]>([
        'Invalid bot type format. Expected format: type/name',
      ]);
    }

    const [botType, _] = type.split('/');

    //Check if it has a schedule
    if (botType === 'scheduled') {
      if (!config.schedule) {
        return Failure<boolean, string[]>(['No schedule provided']);
      }

      if (!isValidCron(config.schedule)) {
        return Failure<boolean, string[]>([
          `${config.schedule} is not a valid cron expression`,
        ]);
      }
    }
    return this.validateWithSchema(customBot, config);
  }

  private validateWithSchema(
    customBot: CustomBot,
    config: any,
  ): Result<boolean, string[]> {
    const {
      config: {
        schema: { bot },
      },
    } = customBot;

    // Filter config if additionalProperties is false
    const configToValidate = this.filterConfigForSchema(config, bot);

    const ajv = new Ajv({ allErrors: true });
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

  private filterConfigForSchema(config: any, schema: any): any {
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

  private filterObjectProperties(obj: any, schema: any): any {
    if (!obj || typeof obj !== 'object' || Array.isArray(obj)) {
      return obj;
    }

    const filtered = {};
    const schemaProperties = schema.properties || {};

    for (const key of Object.keys(schemaProperties)) {
      if (obj[key] !== undefined) {
        const propertySchema = schemaProperties[key];
        
        // If property is an object and has its own schema, recursively filter
        if (propertySchema.type === 'object' && propertySchema.properties) {
          filtered[key] = this.filterObjectProperties(obj[key], propertySchema);
        } else {
          filtered[key] = obj[key];
        }
      }
    }

    return filtered;
  }
}
