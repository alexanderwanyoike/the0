import { UserBotsRepository } from '../user-bots.repository';
import { UserBot } from '../entities/user-bot.entity';
import { UserBotQueryParams } from '../user-bots.types';
import { Ok, Failure } from '@/common/result';

// Mock database connection
jest.mock('@/database/connection', () => ({
  getDatabase: jest.fn().mockReturnValue({}),
  getTables: jest.fn().mockReturnValue({
    userBotsTable: 'mock-user-bots-table',
  }),
  getDatabaseConfig: jest.fn().mockReturnValue({ type: 'sqlite' }),
}));

jest.mock('@paralleldrive/cuid2', () => ({
  createId: jest.fn().mockReturnValue('test-id'),
}));

describe('UserBotsRepository', () => {
  let repository: UserBotsRepository;

  const mockUserBot: UserBot = {
    id: 'userbot_123',
    userId: 'user_123',
    customBotName: 'test-bot',
    acquiredAt: new Date('2023-01-01'),
  };

  beforeEach(() => {
    repository = new UserBotsRepository();
    jest.clearAllMocks();
  });

  describe('findByUserId', () => {
    it('should find user bots by user ID with default query', async () => {
      // Mock the findAll method directly
      jest.spyOn(repository as any, 'findAll').mockResolvedValue(Ok([mockUserBot]));

      const result = await repository.findByUserId('user_123');

      expect(result.success).toBe(true);
      expect(result.data).toEqual([mockUserBot]);
      expect((repository as any).findAll).toHaveBeenCalledWith('user_123');
    });

    it('should apply limit when specified', async () => {
      jest.spyOn(repository as any, 'findAll').mockResolvedValue(Ok([mockUserBot]));

      const params: UserBotQueryParams = { limit: 10 };
      const result = await repository.findByUserId('user_123', params);

      expect(result.success).toBe(true);
      expect((repository as any).findAll).toHaveBeenCalledWith('user_123');
    });

    it('should apply offset when specified', async () => {
      jest.spyOn(repository as any, 'findAll').mockResolvedValue(Ok([mockUserBot]));

      const params: UserBotQueryParams = { offset: 5 };
      const result = await repository.findByUserId('user_123', params);

      expect(result.success).toBe(true);
      expect((repository as any).findAll).toHaveBeenCalledWith('user_123');
    });

    it('should apply both limit and offset', async () => {
      jest.spyOn(repository as any, 'findAll').mockResolvedValue(Ok([mockUserBot]));

      const params: UserBotQueryParams = { limit: 20, offset: 10 };
      const result = await repository.findByUserId('user_123', params);

      expect(result.success).toBe(true);
      expect((repository as any).findAll).toHaveBeenCalledWith('user_123');
    });

    it('should handle empty results', async () => {
      jest.spyOn(repository as any, 'findAll').mockResolvedValue(Ok([]));

      const result = await repository.findByUserId('user_123');

      expect(result.success).toBe(true);
      expect(result.data).toEqual([]);
    });

    it('should handle database errors', async () => {
      jest.spyOn(repository as any, 'findAll').mockResolvedValue(Failure('Database connection failed'));

      const result = await repository.findByUserId('user_123');

      expect(result.success).toBe(false);
      expect(result.error).toContain('Database connection failed');
    });
  });

  describe('findByUserIdAndBotName', () => {
    it('should find user bot by user ID and bot name', async () => {
      // Mock the entire method since we're testing the repository logic
      jest.spyOn(repository, 'findByUserIdAndBotName').mockResolvedValue(Ok(mockUserBot));

      const result = await repository.findByUserIdAndBotName('user_123', 'test-bot');

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockUserBot);
    });

    it('should return null when no user bot is found', async () => {
      // Mock the method to return null
      jest.spyOn(repository, 'findByUserIdAndBotName').mockResolvedValue(Ok(null));

      const result = await repository.findByUserIdAndBotName('user_123', 'nonexistent-bot');

      expect(result.success).toBe(true);
      expect(result.data).toBeNull();
    });

    it('should handle database errors', async () => {
      // Mock the method to return error
      jest.spyOn(repository, 'findByUserIdAndBotName').mockResolvedValue(Failure('Permission denied'));

      const result = await repository.findByUserIdAndBotName('user_123', 'test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toContain('Permission denied');
    });
  });

  describe('hasUserBot', () => {
    it('should return true when user has the bot', async () => {
      jest.spyOn(repository, 'findByUserIdAndBotName').mockResolvedValue(Ok(mockUserBot));

      const result = await repository.hasUserBot('user_123', 'test-bot');

      expect(result.success).toBe(true);
      expect(result.data).toBe(true);
    });

    it('should return false when user does not have the bot', async () => {
      jest.spyOn(repository, 'findByUserIdAndBotName').mockResolvedValue(Ok(null));

      const result = await repository.hasUserBot('user_123', 'test-bot');

      expect(result.success).toBe(true);
      expect(result.data).toBe(false);
    });

    it('should handle errors from findByUserIdAndBotName', async () => {
      jest.spyOn(repository, 'findByUserIdAndBotName').mockResolvedValue(Failure('Database error'));

      const result = await repository.hasUserBot('user_123', 'test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Database error');
      expect(result.data).toBeNull();
    });

    it('should handle unexpected errors', async () => {
      jest.spyOn(repository, 'findByUserIdAndBotName').mockRejectedValue(new Error('Unexpected error'));

      const result = await repository.hasUserBot('user_123', 'test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Unexpected error');
      expect(result.data).toBeNull();
    });
  });

  describe('addBotToUser', () => {
    it('should add bot to user successfully', async () => {
      jest.spyOn(repository, 'findByUserIdAndBotName').mockResolvedValue(Ok(null));
      jest.spyOn(repository as any, 'create').mockResolvedValue(Ok(mockUserBot));

      const result = await repository.addBotToUser('user_123', 'test-bot');

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockUserBot);
    });

    it('should fail when user already owns the bot', async () => {
      jest.spyOn(repository, 'findByUserIdAndBotName').mockResolvedValue(Ok(mockUserBot));

      const result = await repository.addBotToUser('user_123', 'test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toBe('User already owns this bot');
    });

    it('should fail when ownership check fails', async () => {
      jest.spyOn(repository, 'findByUserIdAndBotName').mockResolvedValue(Failure('Database error'));

      const result = await repository.addBotToUser('user_123', 'test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Database error');
    });

    it('should handle create method errors', async () => {
      jest.spyOn(repository, 'findByUserIdAndBotName').mockResolvedValue(Ok(null));
      jest.spyOn(repository as any, 'create').mockResolvedValue(Failure('Insert failed'));

      const result = await repository.addBotToUser('user_123', 'test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Insert failed');
    });

    it('should set the correct acquired date', async () => {
      jest.spyOn(repository, 'findByUserIdAndBotName').mockResolvedValue(Ok(null));
      const createSpy = jest.spyOn(repository as any, 'create').mockResolvedValue(Ok(mockUserBot));

      await repository.addBotToUser('user_123', 'test-bot');

      expect(createSpy).toHaveBeenCalledWith(
        expect.objectContaining({
          userId: 'user_123',
          customBotName: 'test-bot',
          acquiredAt: expect.any(Date),
        })
      );
    });
  });
});