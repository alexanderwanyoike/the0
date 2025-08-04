import { BacktestValidator } from '../backtest.validator';
import { CustomBot } from '@/custom-bot/custom-bot.types';
import { Result } from '@/common';
import Ajv from 'ajv';

// Mock Ajv
jest.mock('ajv');

// Mock ajv-formats
jest.mock('ajv-formats', () => jest.fn());

describe('BacktestValidator', () => {
  let validator: BacktestValidator;
  let mockAjv: jest.Mocked<Ajv>;
  let mockValidate: jest.Mock & { errors?: any[] | null };

  const mockCustomBot: CustomBot = {
    id: 'test-bot-id',
    name: 'test-bot',
    version: '1.0.0',
    config: {
      name: 'test-bot',
      description: 'Test bot',
      version: '1.0.0',
      type: 'scheduled',
      runtime: 'python3.11',
      author: 'test-author',
      entrypoints: {
        bot: 'main.py',
        backtest: 'backtest.py',
      },
      schema: {
        backtest: {
          type: 'object',
          properties: {
            type: { type: 'string' },
            symbol: { type: 'string' },
            amount: { type: 'number' },
          },
          required: ['type', 'symbol', 'amount'],
        },
        bot: {
          type: 'object',
          properties: {
            type: { type: 'string' },
          },
          required: ['type'],
        },
      },
      readme: 'Test bot readme',
    },
    filePath: 'gs://bucket/test-bot/1.0.0/bot.zip',
    userId: 'test-user-id',
    status: 'approved',
    createdAt: new Date(),
    updatedAt: new Date(),
  };

  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();

    // Mock the validate function
    mockValidate = jest.fn();

    // Mock Ajv constructor and methods
    mockAjv = {
      compile: jest.fn().mockReturnValue(mockValidate),
      addFormat: jest.fn(),
      addKeyword: jest.fn(),
      addSchema: jest.fn(),
      getSchema: jest.fn(),
      removeKeyword: jest.fn(),
      removeSchema: jest.fn(),
      validate: jest.fn(),
      validateSchema: jest.fn(),
    } as any;

    (Ajv as unknown as jest.Mock).mockImplementation(() => mockAjv);

    validator = new BacktestValidator();
  });

  describe('validate', () => {
    const validConfig = {
      type: 'scheduled/test-bot',
      symbol: 'BTC/USD',
      amount: 1000,
    };

    it('should return failure when no type is provided', async () => {
      const config = { symbol: 'BTC/USD', amount: 1000 };

      const result = await validator.validate(config, mockCustomBot);

      expect(result.success).toBe(false);
      expect(result.error).toEqual(['No type provided']);
    });

    it('should return failure when type format is invalid', async () => {
      const config = {
        type: 'invalid-type-format',
        symbol: 'BTC/USD',
        amount: 1000,
      };

      const result = await validator.validate(config, mockCustomBot);

      expect(result.success).toBe(false);
      expect(result.error).toEqual([
        'Invalid bot type format. Expected format: type/name',
      ]);
    });

    it('should return failure when bot type is not supported', async () => {
      const config = {
        type: 'unsupported/test-bot',
        symbol: 'BTC/USD',
        amount: 1000,
      };

      const result = await validator.validate(config, mockCustomBot);

      expect(result.success).toBe(false);
      expect(result.error).toEqual([
        'Invalid bot type. Supported types are: scheduled, realtime, event',
      ]);
    });

    it('should return success when validation passes', async () => {
      mockValidate.mockReturnValue(true);

      const result = await validator.validate(validConfig, mockCustomBot);

      expect(result.success).toBe(true);
      expect(result.data).toBe(true);
      expect(mockAjv.compile).toHaveBeenCalledWith(
        mockCustomBot.config.schema.backtest,
      );
      expect(mockValidate).toHaveBeenCalledWith(validConfig);
    });

    it('should return failure when schema validation fails', async () => {
      const mockErrors = [
        {
          instancePath: '/symbol',
          schemaPath: '#/properties/symbol/type',
          keyword: 'type',
          params: { type: 'string' },
          message: 'must be string',
        },
        {
          instancePath: '',
          schemaPath: '#/required',
          keyword: 'required',
          params: { missingProperty: 'amount' },
          message: "must have required property 'amount'",
        },
      ];

      mockValidate.mockReturnValue(false);
      mockValidate.errors = mockErrors;

      const result = await validator.validate(validConfig, mockCustomBot);

      expect(result.success).toBe(false);
      expect(result.error).toEqual([
        '/symbol must be string',
        "must have required property 'amount'",
      ]);
    });

    it('should handle errors without instancePath', async () => {
      const mockErrors = [
        {
          instancePath: '',
          schemaPath: '#/required',
          keyword: 'required',
          params: { missingProperty: 'type' },
          message: "must have required property 'type'",
        },
      ];

      mockValidate.mockReturnValue(false);
      mockValidate.errors = mockErrors;

      const result = await validator.validate(validConfig, mockCustomBot);

      expect(result.success).toBe(false);
      expect(result.error).toEqual(["must have required property 'type'"]);
    });

    it('should handle empty errors array', async () => {
      mockValidate.mockReturnValue(false);
      mockValidate.errors = [];

      const result = await validator.validate(validConfig, mockCustomBot);

      expect(result.success).toBe(false);
      expect(result.error).toEqual([]);
    });

    it('should handle null errors', async () => {
      mockValidate.mockReturnValue(false);
      mockValidate.errors = null;

      const result = await validator.validate(validConfig, mockCustomBot);

      expect(result.success).toBe(false);
      expect(result.error).toEqual([]);
    });

    it('should work with all supported bot types', async () => {
      const botTypes = ['scheduled', 'realtime', 'event'];

      for (const botType of botTypes) {
        mockValidate.mockReturnValue(true);

        const config = {
          type: `${botType}/test-bot`,
          symbol: 'BTC/USD',
          amount: 1000,
        };
        const result = await validator.validate(config, mockCustomBot);

        expect(result.success).toBe(true);
        expect(result.data).toBe(true);
      }
    });

    it('should handle complex bot names with hyphens', async () => {
      mockValidate.mockReturnValue(true);

      const config = {
        type: 'scheduled/my-complex-bot-name',
        symbol: 'BTC/USD',
        amount: 1000,
      };
      const result = await validator.validate(config, mockCustomBot);

      expect(result.success).toBe(true);
      expect(result.data).toBe(true);
    });

    it('should use the correct backtest schema from custom bot', async () => {
      mockValidate.mockReturnValue(true);

      const customBotWithDifferentSchema = {
        ...mockCustomBot,
        config: {
          ...mockCustomBot.config,
          schema: {
            backtest: {
              type: 'object',
              properties: {
                type: { type: 'string' },
                strategy: { type: 'string' },
                timeframe: { type: 'string' },
              },
              required: ['type', 'strategy', 'timeframe'],
            },
            bot: {
              type: 'object',
              properties: {
                type: { type: 'string' },
              },
              required: ['type'],
            },
          },
        },
      };

      const result = await validator.validate(
        validConfig,
        customBotWithDifferentSchema,
      );

      expect(result.success).toBe(true);
      expect(mockAjv.compile).toHaveBeenCalledWith(
        customBotWithDifferentSchema.config.schema.backtest,
      );
    });

    it('should create new Ajv instance with allErrors: true', async () => {
      mockValidate.mockReturnValue(true);

      await validator.validate(validConfig, mockCustomBot);

      expect(Ajv).toHaveBeenCalledWith({ allErrors: true });
    });

    it('should handle mixed case in bot type validation', async () => {
      // Bot type should be validated as lowercase
      const config = {
        type: 'Scheduled/test-bot',
        symbol: 'BTC/USD',
        amount: 1000,
      };

      const result = await validator.validate(config, mockCustomBot);

      expect(result.success).toBe(false);
      expect(result.error).toEqual([
        'Invalid bot type format. Expected format: type/name',
      ]);
    });

    it('should handle validation with numeric and special characters in bot names', async () => {
      mockValidate.mockReturnValue(true);

      const config = {
        type: 'scheduled/bot-123',
        symbol: 'BTC/USD',
        amount: 1000,
      };
      const result = await validator.validate(config, mockCustomBot);

      expect(result.success).toBe(true);
      expect(result.data).toBe(true);
    });

    it('should reject bot type with invalid characters', async () => {
      const config = {
        type: 'scheduled/bot_invalid',
        symbol: 'BTC/USD',
        amount: 1000,
      };

      const result = await validator.validate(config, mockCustomBot);

      expect(result.success).toBe(false);
      expect(result.error).toEqual([
        'Invalid bot type format. Expected format: type/name',
      ]);
    });

    it('should reject bot type with uppercase characters', async () => {
      const config = {
        type: 'scheduled/BotName',
        symbol: 'BTC/USD',
        amount: 1000,
      };

      const result = await validator.validate(config, mockCustomBot);

      expect(result.success).toBe(false);
      expect(result.error).toEqual([
        'Invalid bot type format. Expected format: type/name',
      ]);
    });

    it('should reject bot type without slash separator', async () => {
      const config = { type: 'scheduledbot', symbol: 'BTC/USD', amount: 1000 };

      const result = await validator.validate(config, mockCustomBot);

      expect(result.success).toBe(false);
      expect(result.error).toEqual([
        'Invalid bot type format. Expected format: type/name',
      ]);
    });

    it('should reject bot type with multiple slashes', async () => {
      const config = {
        type: 'scheduled/test/bot',
        symbol: 'BTC/USD',
        amount: 1000,
      };

      const result = await validator.validate(config, mockCustomBot);

      expect(result.success).toBe(false);
      expect(result.error).toEqual([
        'Invalid bot type format. Expected format: type/name',
      ]);
    });

    it('should handle edge case with empty bot name', async () => {
      const config = { type: 'scheduled/', symbol: 'BTC/USD', amount: 1000 };

      const result = await validator.validate(config, mockCustomBot);

      expect(result.success).toBe(false);
      expect(result.error).toEqual([
        'Invalid bot type format. Expected format: type/name',
      ]);
    });

    it('should handle edge case with empty bot type', async () => {
      const config = { type: '/test-bot', symbol: 'BTC/USD', amount: 1000 };

      const result = await validator.validate(config, mockCustomBot);

      expect(result.success).toBe(false);
      expect(result.error).toEqual([
        'Invalid bot type format. Expected format: type/name',
      ]);
    });
  });

  describe('validateWithSchema', () => {
    it('should be called internally by validate method', async () => {
      const validConfig = {
        type: 'scheduled/test-bot',
        symbol: 'BTC/USD',
        amount: 1000,
      };

      mockValidate.mockReturnValue(true);

      const result = await validator.validate(validConfig, mockCustomBot);

      expect(result.success).toBe(true);
      expect(mockAjv.compile).toHaveBeenCalledWith(
        mockCustomBot.config.schema.backtest,
      );
      expect(mockValidate).toHaveBeenCalledWith(validConfig);
    });
  });
});
