import { Test, TestingModule } from '@nestjs/testing';
import { BotValidator } from '../bot.validator';
import { Failure, Ok } from '@/common';
import { CustomBot } from '@/custom-bot/custom-bot.types';

describe('BotValidator', () => {
  let validator: BotValidator;
  let customBot: CustomBot;
  let nonScheduledCustomBot: CustomBot;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [BotValidator],
    }).compile();

    validator = module.get<BotValidator>(BotValidator);
    customBot = {
      id: 'test-bot',
      name: 'test-bot',
      version: '1.0.0',
      status: 'pending_review',
      config: {
        name: 'test-bot',
        description: 'A test bot',
        version: '1.0.0',
        type: 'scheduled',
        runtime: 'python3.11',
        author: 'test-author',
        entrypoints: {
          bot: 'bot.py',
          backtest: 'backtest.py',
        },
        schema: {
          backtest: {},
          bot: {
            type: 'object',
            properties: {
              foo: { type: 'string' },
              bar: { type: 'number' },
            },
            required: ['foo', 'bar'],
          },
        },
        readme: 'This is a test bot',
        metadata: {
          categories: ['test'],
          instruments: ['BTC'],
          exchanges: ['Binance'],
          tags: ['test', 'bot'],
        },
      },
      filePath: 'gs://test-bucket/test-bot',
      userId: 'test-user',
      createdAt: new Date(),
      updatedAt: new Date(),
    };

    nonScheduledCustomBot = {
      id: 'test-non-scheduled-bot',
      name: 'test-non-scheduled-bot',
      version: '1.0.0',
      status: 'pending_review',
      config: {
        name: 'test-bot',
        description: 'A test bot',
        version: '1.0.0',
        type: 'event',
        runtime: 'python3.11',
        author: 'test-author',
        entrypoints: {
          bot: 'bot.py',
          backtest: 'backtest.py',
        },
        schema: {
          backtest: {},
          bot: {
            type: 'object',
            properties: {
              foo: { type: 'string' },
              bar: { type: 'number' },
            },
            required: ['foo', 'bar'],
          },
        },
        readme: 'This is a test bot',
        metadata: {
          categories: ['test'],
          instruments: ['BTC'],
          exchanges: ['Binance'],
          tags: ['test', 'bot'],
        },
      },
      filePath: 'gs://test-bucket/test-bot',
      userId: 'test-user',
      createdAt: new Date(),
      updatedAt: new Date(),
    };
  });

  describe('bot type validation', () => {
    it('should return an error if the bot type string is invalid', async () => {
      // Arrange
      const config = { type: 'invalid-bot-type' };
      const result = await validator.validate(config, customBot);

      // Assert
      expect(result).toEqual(
        Failure<boolean, string[]>([
          'Invalid bot type format. Expected format: type/name',
        ]),
      );
    });
  });

  describe('scheduled bots', () => {
    it('should always have a schedule in the configuration', async () => {
      //Arrange
      const config = { type: 'scheduled/test-bot', foo: 'bar' };

      const result = await validator.validate(config, customBot);

      //Assert
      expect(result).toEqual(
        Failure<boolean, string[]>(['No schedule provided']),
      );
    });

    it('should validate that the schedule is correctly formatted', async () => {
      // Arrange
      const config = {
        type: 'scheduled/test-bot',
        schedule: 'invalid-schedule',
        foo: 'bar',
      };

      const result = await validator.validate(config, customBot);

      // Assert
      expect(result).toEqual(
        Failure<boolean, string[]>([
          `${config.schedule} is not a valid cron expression`,
        ]),
      );
    });

    it('should not validate the schedule if the bot type is not scheduled', async () => {
      // Arrange
      const config = {
        type: 'other/test-bot',
        foo: 'bar',
        bar: 42,
      };

      const result = await validator.validate(config, nonScheduledCustomBot);

      // Assert
      expect(result).toEqual(Ok<boolean, string[]>(true));
    });
  });

  describe('validation against schema', () => {
    it('should validate successfully if config is valid (scheduled)', async () => {
      const config = {
        type: 'scheduled/test-bot',
        schedule: '*/5 * * * *',
        foo: 'bar',
        bar: 42,
      };

      const result = await validator.validate(config, customBot);

      expect(result).toEqual(Ok<boolean, string[]>(true));
    });

    it('should return an error if config does not match schema (scheduled)', async () => {
      const config = {
        type: 'scheduled/test-bot',
        schedule: '*/5 * * * *',
        foo: 123,
      };

      const result = await validator.validate(config, customBot);

      expect(result).toEqual(
        Failure<boolean, string[]>([
          "must have required property 'bar'",
          '/foo must be string',
        ]),
      );
    });

    it('should validate successfully if config is valid (non-scheduled)', async () => {
      const config = {
        type: 'other/test-bot',
        foo: 'bar',
        bar: 42,
      };

      const result = await validator.validate(config, nonScheduledCustomBot);

      expect(result).toEqual(Ok<boolean, string[]>(true));
    });

    it('should return an error if config does not match schema (non-scheduled)', async () => {
      const config = {
        type: 'other/test-bot',
        foo: 123,
      };

      const result = await validator.validate(config, nonScheduledCustomBot);

      expect(result).toEqual(
        Failure<boolean, string[]>([
          "must have required property 'bar'",
          '/foo must be string',
        ]),
      );
    });
  });
});
