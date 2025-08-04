import { Test, TestingModule } from '@nestjs/testing';
import { BotRepository } from '../bot.repository';
import { Ok, Failure } from '@/common/result';

describe('BotRepository', () => {
  let repository: BotRepository;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [BotRepository],
    }).compile();

    repository = module.get<BotRepository>(BotRepository);

    // Reset all mocks
    jest.clearAllMocks();
  });

  describe('create', () => {
    const botData = {
      name: 'Test Bot',
      config: {
        type: 'scheduled/test-bot',
        version: '1.0.0',
        foo: 'test',
        bar: 123,
      },
      userId: 'user-123',
      topic: 'the0-scheduled-custom-bot',
      customBotId: 'custom-bot-123',
    };

    it('should create a bot successfully', async () => {
      const mockBotResult = {
        id: 'test-bot-id',
        ...botData,
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      jest.spyOn(repository, 'create').mockResolvedValue(Ok(mockBotResult));

      const result = await repository.create(botData);

      expect(result.success).toBe(true);
      expect(result.data.id).toBe('test-bot-id');
      expect(result.data.name).toBe('Test Bot');
      expect(result.data.userId).toBe('user-123');
    });

    it('should handle creation errors', async () => {
      jest.spyOn(repository, 'create').mockResolvedValue(Failure('Database error'));

      const result = await repository.create(botData);

      expect(result.success).toBe(false);
      expect(result.error).toBe('Database error');
    });
  });

  describe('findAll', () => {
    const userId = 'user-123';

    it('should find all bots for a user', async () => {
      const mockBots = [
        {
          id: 'bot-1',
          name: 'Bot 1',
          userId: 'user-123',
          config: { type: 'scheduled/bot1', version: '1.0.0' },
          topic: 'the0-scheduled-custom-bot',
          customBotId: 'custom-bot-1',
          createdAt: new Date(),
          updatedAt: new Date(),
        },
        {
          id: 'bot-2',
          name: 'Bot 2',
          userId: 'user-123',
          config: { type: 'scheduled/bot2', version: '1.0.0' },
          topic: 'the0-scheduled-custom-bot',
          customBotId: 'custom-bot-2',
          createdAt: new Date(),
          updatedAt: new Date(),
        },
      ];

      jest.spyOn(repository, 'findAll').mockResolvedValue(Ok(mockBots));

      const result = await repository.findAll(userId);

      expect(result.success).toBe(true);
      expect(result.data).toHaveLength(2);
      expect(result.data[0].name).toBe('Bot 1');
      expect(result.data[1].name).toBe('Bot 2');
    });

    it('should return empty array when no bots exist', async () => {
      jest.spyOn(repository, 'findAll').mockResolvedValue(Ok([]));

      const result = await repository.findAll(userId);

      expect(result.success).toBe(true);
      expect(result.data).toEqual([]);
    });

    it('should handle query errors', async () => {
      jest.spyOn(repository, 'findAll').mockResolvedValue(Failure('Query error'));

      const result = await repository.findAll(userId);

      expect(result.success).toBe(false);
      expect(result.error).toBe('Query error');
    });
  });

  describe('findOne', () => {
    const userId = 'user-123';
    const botId = 'bot-123';

    it('should find a specific bot', async () => {
      const mockBot = {
        id: botId,
        name: 'Test Bot',
        userId: userId,
        config: {
          type: 'scheduled/test',
          version: '1.0.0',
        },
        topic: 'the0-scheduled-custom-bot',
        customBotId: 'custom-bot-123',
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      jest.spyOn(repository, 'findOne').mockResolvedValue(Ok(mockBot));

      const result = await repository.findOne(userId, botId);

      expect(result.success).toBe(true);
      expect(result.data.id).toBe(botId);
      expect(result.data.name).toBe('Test Bot');
      expect(result.data.userId).toBe(userId);
    });

    it('should return failure when bot not found', async () => {
      jest.spyOn(repository, 'findOne').mockResolvedValue(Failure('Not found'));

      const result = await repository.findOne(userId, botId);

      expect(result.success).toBe(false);
      expect(result.error).toBe('Not found');
    });

    it('should handle query errors', async () => {
      jest.spyOn(repository, 'findOne').mockResolvedValue(Failure('Database error'));

      const result = await repository.findOne(userId, botId);

      expect(result.success).toBe(false);
      expect(result.error).toBe('Database error');
    });
  });

  describe('update', () => {
    const userId = 'user-123';
    const botId = 'bot-123';
    const updateData = {
      name: 'Updated Bot',
      config: {
        type: 'scheduled/updated',
        version: '1.1.0',
        foo: 'updated',
      },
    };

    it('should update a bot successfully', async () => {
      const updatedBot = {
        id: botId,
        ...updateData,
        userId: userId,
        topic: 'the0-scheduled-custom-bot',
        customBotId: 'custom-bot-123',
        createdAt: new Date('2024-01-01'),
        updatedAt: new Date(),
      };

      jest.spyOn(repository, 'update').mockResolvedValue(Ok(updatedBot));

      const result = await repository.update(userId, botId, updateData);

      expect(result.success).toBe(true);
      expect(result.data.id).toBe(botId);
      expect(result.data.name).toBe('Updated Bot');
      expect(result.data.config.version).toBe('1.1.0');
    });

    it('should return failure when bot not found', async () => {
      jest.spyOn(repository, 'update').mockResolvedValue(Failure('Not found'));

      const result = await repository.update(userId, botId, updateData);

      expect(result.success).toBe(false);
      expect(result.error).toBe('Not found');
    });

    it('should handle update errors', async () => {
      jest.spyOn(repository, 'update').mockResolvedValue(Failure('Update error'));

      const result = await repository.update(userId, botId, updateData);

      expect(result.success).toBe(false);
      expect(result.error).toBe('Update error');
    });
  });

  describe('remove', () => {
    const userId = 'user-123';
    const botId = 'bot-123';

    it('should remove a bot successfully', async () => {
      jest.spyOn(repository, 'remove').mockResolvedValue(Ok(undefined));

      const result = await repository.remove(userId, botId);

      expect(result.success).toBe(true);
    });

    it('should return failure when bot not found', async () => {
      jest.spyOn(repository, 'remove').mockResolvedValue(Failure('Not found'));

      const result = await repository.remove(userId, botId);

      expect(result.success).toBe(false);
      expect(result.error).toBe('Not found');
    });

    it('should handle deletion errors', async () => {
      jest.spyOn(repository, 'remove').mockResolvedValue(Failure('Deletion error'));

      const result = await repository.remove(userId, botId);

      expect(result.success).toBe(false);
      expect(result.error).toBe('Deletion error');
    });
  });
});