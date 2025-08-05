import { Test, TestingModule } from '@nestjs/testing';
import { BotController } from '../bot.controller';
import { BotService } from '../bot.service';
import { REQUEST } from '@nestjs/core';
import { BotRepository } from '../bot.repository';
import { Bot } from '../entities/bot.entity';
import {
  BadRequestException,
  CanActivate,
  NotFoundException,
} from '@nestjs/common';
import { BotValidator } from '../bot.validator';
import { ConfigModule } from '@nestjs/config';
import { CustomBotService } from '@/custom-bot/custom-bot.service';
import { AuthCombinedGuard } from '@/auth/auth-combined.guard';
import { ApiKeyService } from '@/api-key/api-key.service';
import { NatsService } from '@/nats/nats.service';
// FeatureGateService removed for OSS version
import { Ok } from '@/common/result';

describe('BotController - Enhanced Tests', () => {
  let controller: BotController;
  let service: BotService;
  let repository: BotRepository;
  let validator: BotValidator;
  let customBotService: CustomBotService;
  const uid = 'test-user-id';

  const mockCustomBot = {
    id: 'test-custom-bot',
    name: 'test-custom-bot',
    version: '1.0.0',
    filePath: 'gs://test-bucket/test-custom-bot',
    userId: 'another-user-id',
    status: 'approved',
    createdAt: new Date(),
    updatedAt: new Date(),
    config: {
      name: 'test-custom-bot',
      type: 'scheduled',
      runtime: 'python3.11',
      description: 'A test custom bot',
      entrypoints: {
        bot: 'bot.py',
        backtest: 'backtest.py',
      },
      schema: {
        bot: {
          type: 'object',
          properties: {
            foo: { type: 'string' },
            bar: { type: 'number' },
          },
          required: ['foo', 'bar'],
        },
      },
      readme: 'This is a test custom bot',
      metadata: {
        categories: ['test'],
        instruments: ['BTC'],
        exchanges: ['Binance'],
        tags: ['test', 'bot'],
      },
    },
    marketplace: {
      isPublished: true,
      price: 0,
    },
  };


  const mockCustomBotService = {
    getGlobalSpecificVersion: jest.fn().mockResolvedValue({
      success: true,
      error: null,
      data: mockCustomBot,
    }),
    getUserSpecificVersion: jest.fn().mockResolvedValue({
      success: true,
      error: null,
      data: mockCustomBot,
    }),
    createCustomBot: jest.fn(),
    updateCustomBot: jest.fn(),
    getAllUserVersions: jest.fn(),
    getAllGlobalVersions: jest.fn(),
    getAllUserSpecificVersions: jest.fn(),
  };

  beforeEach(async () => {
    const mockForceGuard: CanActivate = { canActivate: jest.fn(() => true) };

    const module: TestingModule = await Test.createTestingModule({
      imports: [
        ConfigModule,
      ],
      controllers: [BotController],
      providers: [
        BotService,
        BotRepository,
        BotValidator,
        {
          provide: CustomBotService,
          useValue: mockCustomBotService,
        },
        {
          provide: ApiKeyService,
          useValue: {},
        },
        {
          provide: NatsService,
          useValue: {
            publish: jest.fn().mockResolvedValue(Ok(null)),
          },
        },
        {
          provide: REQUEST,
          useValue: { user: { uid } },
        },
        // FeatureGateService removed for OSS version
      ],
    })
      .overrideGuard(AuthCombinedGuard)
      .useValue(mockForceGuard)
      .compile();

    repository = module.get<BotRepository>(BotRepository);
    controller = await module.resolve<BotController>(BotController);
    service = await module.resolve<BotService>(BotService);
    validator = module.get<BotValidator>(BotValidator);
    customBotService = module.get<CustomBotService>(CustomBotService);

    // Reset all mocks
    jest.clearAllMocks();
  });

  describe('create', () => {
    const validBot = {
      name: 'Test Bot',
      config: {
        type: 'scheduled/test-bot',
        version: '1.0.0',
        foo: 'test',
        bar: 123,
      },
    };

    beforeEach(() => {
      validator.validate = jest.fn().mockReturnValue({
        success: true,
        error: null,
        data: null,
      });

      jest.spyOn(repository, 'findAll').mockResolvedValue({
        success: true,
        error: null,
        data: [],
      });

      jest.spyOn(repository, 'create').mockResolvedValue({
        success: true,
        error: null,
        data: {
          id: 'test-id',
          ...validBot,
          userId: uid,
          topic: 'the0-scheduled-custom-bot',
        } as Bot,
      });
    });

    it('should create a bot successfully', async () => {
      jest.spyOn(repository, 'findOne').mockResolvedValue({
        success: true,
        error: null,
        data: {
          id: 'test-id',
          ...validBot,
          userId: uid,
          topic: 'the0-scheduled-custom-bot',
        } as Bot,
      });
      const result = await controller.create(validBot);

      expect(result).toEqual({
        id: 'test-id',
        ...validBot,
        userId: uid,
        topic: 'the0-scheduled-custom-bot',
      });
      expect(repository.create).toHaveBeenCalledWith({
        ...validBot,
        userId: uid,
        topic: 'the0-scheduled-custom-bot',
        customBotId: mockCustomBot.id,
      });
    });

    it('should throw BadRequestException when service fails', async () => {
      jest.spyOn(repository, 'create').mockResolvedValue({
        success: false,
        error: 'Creation failed',
        data: null,
      });

      await expect(controller.create(validBot)).rejects.toThrow(
        BadRequestException,
      );
    });

    it('should throw BadRequestException for invalid bot type format', async () => {
      const invalidBot = {
        ...validBot,
        config: { ...validBot.config, type: 'invalid-format' },
      };

      await expect(controller.create(invalidBot)).rejects.toThrow(
        BadRequestException,
      );
    });

    it('should throw BadRequestException for invalid version format', async () => {
      const invalidBot = {
        ...validBot,
        config: { ...validBot.config, version: 'invalid-version' },
      };

      await expect(controller.create(invalidBot)).rejects.toThrow(
        BadRequestException,
      );
    });

    it('should throw BadRequestException when bot limit exceeded', async () => {
      jest.spyOn(repository, 'findAll').mockResolvedValue({
        success: true,
        error: null,
        data: [{ id: 'bot1' }, { id: 'bot2' }, { id: 'bot3' }] as Bot[],
      });

      await expect(controller.create(validBot)).rejects.toThrow(
        BadRequestException,
      );
    });

    it('should throw BadRequestException when custom bot not found', async () => {
      mockCustomBotService.getGlobalSpecificVersion = jest
        .fn()
        .mockResolvedValue({
          success: false,
          error: 'Bot not found',
          data: null,
        });

      await expect(controller.create(validBot)).rejects.toThrow(
        BadRequestException,
      );
    });

    it('should throw BadRequestException when validation fails', async () => {
      validator.validate = jest.fn().mockReturnValue({
        success: false,
        error: ['Invalid configuration'],
        data: null,
      });

      await expect(controller.create(validBot)).rejects.toThrow(
        BadRequestException,
      );
    });

    // Test removed - UserBotsService dependency not needed in OSS version
  });

  describe('findAll', () => {
    it('should return all bots successfully', async () => {
      const bots = [
        { id: 'bot1', name: 'Bot 1' },
        { id: 'bot2', name: 'Bot 2' },
      ] as Bot[];

      jest.spyOn(repository, 'findAll').mockResolvedValue({
        success: true,
        error: null,
        data: bots,
      });

      const result = await controller.findAll();

      expect(result).toEqual(bots);
      expect(repository.findAll).toHaveBeenCalledWith(uid);
    });

    it('should throw NotFoundException when service fails', async () => {
      jest.spyOn(repository, 'findAll').mockResolvedValue({
        success: false,
        error: 'Database error',
        data: null,
      });

      await expect(controller.findAll()).rejects.toThrow(NotFoundException);
    });

    it('should return empty array when no bots exist', async () => {
      jest.spyOn(repository, 'findAll').mockResolvedValue({
        success: true,
        error: null,
        data: [],
      });

      const result = await controller.findAll();

      expect(result).toEqual([]);
    });
  });

  describe('findOne', () => {
    const mockBot = {
      id: 'test-id',
      name: 'Test Bot',
      config: { type: 'scheduled/test-bot', version: '1.0.0' },
    } as Bot;

    it('should return a bot successfully', async () => {
      jest.spyOn(repository, 'findOne').mockResolvedValue({
        success: true,
        error: null,
        data: mockBot,
      });

      const result = await controller.findOne('test-id');

      expect(result).toEqual(mockBot);
      expect(repository.findOne).toHaveBeenCalledWith(uid, 'test-id');
    });

    it('should throw NotFoundException when bot not found', async () => {
      jest.spyOn(repository, 'findOne').mockResolvedValue({
        success: false,
        error: 'Bot not found',
        data: null,
      });

      await expect(controller.findOne('nonexistent-id')).rejects.toThrow(
        NotFoundException,
      );
    });

    it('should handle invalid bot ID format', async () => {
      jest.spyOn(repository, 'findOne').mockResolvedValue({
        success: false,
        error: 'Invalid ID format',
        data: null,
      });

      await expect(controller.findOne('invalid-id')).rejects.toThrow(
        NotFoundException,
      );
    });
  });

  describe('update', () => {
    const updateData = {
      name: 'Updated Bot',
      config: {
        type: 'scheduled/test-bot',
        version: '1.0.0',
        foo: 'updated',
        bar: 456,
      },
    };

    beforeEach(() => {
      validator.validate = jest.fn().mockReturnValue({
        success: true,
        error: null,
        data: null,
      });
    });

    it('should update a bot successfully', async () => {
      const updatedBot = { id: 'test-id', ...updateData } as Bot;

      // Reset the mock to return success for this test
      mockCustomBotService.getGlobalSpecificVersion = jest.fn().mockResolvedValue({
        success: true,
        error: null,
        data: mockCustomBot,
      });

      jest.spyOn(repository, 'update').mockResolvedValue({
        success: true,
        error: null,
        data: updatedBot,
      });

      const result = await controller.update('test-id', updateData);

      expect(result).toEqual(updatedBot);
      expect(repository.update).toHaveBeenCalledWith(
        uid,
        'test-id',
        updateData,
      );
    });

    it('should throw BadRequestException when update fails', async () => {
      jest.spyOn(repository, 'update').mockResolvedValue({
        success: false,
        error: 'Update failed',
        data: null,
      });

      await expect(controller.update('test-id', updateData)).rejects.toThrow(
        BadRequestException,
      );
    });

    it('should throw BadRequestException when validation fails', async () => {
      validator.validate = jest.fn().mockReturnValue({
        success: false,
        error: ['Invalid configuration'],
        data: null,
      });

      await expect(controller.update('test-id', updateData)).rejects.toThrow(
        BadRequestException,
      );
    });

    it('should throw BadRequestException for invalid bot type on update', async () => {
      mockCustomBotService.getGlobalSpecificVersion = jest
        .fn()
        .mockResolvedValue({
          success: false,
          error: 'Bot type not found',
          data: null,
        });

      await expect(controller.update('test-id', updateData)).rejects.toThrow(
        BadRequestException,
      );
    });
  });

  describe('remove', () => {
    it('should remove a bot successfully', async () => {
      const mockBot = {
        id: 'test-id',
        name: 'Test Bot',
        config: { type: 'scheduled/test-bot', version: '1.0.0' },
        topic: 'the0-scheduled-custom-bot',
        userId: uid,
        createdAt: new Date(),
        updatedAt: new Date(),
        customBotId: 'custom-bot-123',
      } as Bot;

      jest.spyOn(repository, 'findOne').mockResolvedValue({
        success: true,
        error: null,
        data: mockBot,
      });

      jest.spyOn(repository, 'remove').mockResolvedValue({
        success: true,
        error: null,
        data: null,
      });

      await expect(controller.remove('test-id')).resolves.toBeUndefined();
      expect(repository.remove).toHaveBeenCalledWith(uid, 'test-id');
    });

    it('should throw BadRequestException when removal fails', async () => {
      jest.spyOn(repository, 'remove').mockResolvedValue({
        success: false,
        error: 'Bot not found',
        data: null,
      });

      await expect(controller.remove('test-id')).rejects.toThrow(
        BadRequestException,
      );
    });

    it('should handle unauthorized removal attempt', async () => {
      jest.spyOn(repository, 'remove').mockResolvedValue({
        success: false,
        error: 'Unauthorized',
        data: null,
      });

      await expect(controller.remove('test-id')).rejects.toThrow(
        BadRequestException,
      );
    });
  });
});
