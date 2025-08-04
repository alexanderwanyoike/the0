import { UserBotsService } from '../user-bots.service';
import { UserBotsRepository } from '../user-bots.repository';
import { CustomBotService } from '@/custom-bot/custom-bot.service';
import { Ok, Failure } from '@/common/result';
import { UserBotQueryParams } from '../user-bots.types';
import { CustomBotWithVersions } from '@/custom-bot/custom-bot.types';

describe('UserBotsService', () => {
  let service: UserBotsService;
  let mockUserBotsRepository: jest.Mocked<UserBotsRepository>;
  let mockCustomBotService: jest.Mocked<CustomBotService>;
  let mockRequest: any;

  const mockUserBot = {
    id: 'userbot_123',
    customBotName: 'test-bot',
    acquiredAt: new Date('2023-01-01'),
  };

  const mockCustomBotVersions = {
    id: 'bot_123',
    userId: 'developer_123',
    versions: [
      {
        id: 'version_123',
        status: 'published',
        marketplace: {
          price: 0,
          isPublished: true,
        },
      },
    ],
  } as unknown as CustomBotWithVersions;

  beforeEach(() => {
    mockRequest = {
      user: { uid: 'current_user_123' },
    };

    mockUserBotsRepository = {
      findByUserId: jest.fn(),
      addBotToUser: jest.fn(),
      hasUserBot: jest.fn(),
    } as any;

    mockCustomBotService = {
      getAllGlobalVersions: jest.fn(),
      // Removed incrementBotInstalls - marketplace functionality not in OSS version
    } as any;

    service = new UserBotsService(
      mockRequest,
      mockUserBotsRepository,
      mockCustomBotService,
    );
  });

  describe('getUserBots', () => {
    it('should return user bots successfully', async () => {
      mockUserBotsRepository.findByUserId.mockResolvedValue(
        Ok([mockUserBot as any]),
      );
      mockCustomBotService.getAllGlobalVersions.mockResolvedValue(
        Ok(mockCustomBotVersions),
      );

      const result = await service.getUserBots();

      expect(result.success).toBe(true);
      expect(result.data).toEqual([
        {
          id: 'userbot_123',
          customBotName: 'test-bot',
          acquiredAt: new Date('2023-01-01'),
          customBot: mockCustomBotVersions,
        },
      ]);
      expect(mockUserBotsRepository.findByUserId).toHaveBeenCalledWith(
        'current_user_123',
        undefined,
      );
      expect(mockCustomBotService.getAllGlobalVersions).toHaveBeenCalledWith(
        'test-bot',
      );
    });

    it('should return user bots with query parameters', async () => {
      const params: UserBotQueryParams = { limit: 10, offset: 5 };
      mockUserBotsRepository.findByUserId.mockResolvedValue(
        Ok([mockUserBot as any]),
      );
      mockCustomBotService.getAllGlobalVersions.mockResolvedValue(
        Ok(mockCustomBotVersions),
      );

      const result = await service.getUserBots(params);

      expect(result.success).toBe(true);
      expect(mockUserBotsRepository.findByUserId).toHaveBeenCalledWith(
        'current_user_123',
        params,
      );
      expect(mockCustomBotService.getAllGlobalVersions).toHaveBeenCalledWith(
        'test-bot',
      );
    });

    it('should handle empty results', async () => {
      mockUserBotsRepository.findByUserId.mockResolvedValue(Ok([]));

      const result = await service.getUserBots();

      expect(result.success).toBe(true);
      expect(result.data).toEqual([]);
    });

    it('should fail when repository returns error', async () => {
      mockUserBotsRepository.findByUserId.mockResolvedValue(
        Failure('Database error'),
      );

      const result = await service.getUserBots();

      expect(result.success).toBe(false);
      expect(result.error).toBe('Database error');
    });

    it('should handle unexpected errors', async () => {
      mockUserBotsRepository.findByUserId.mockRejectedValue(
        new Error('Unexpected error'),
      );

      const consoleSpy = jest.spyOn(console, 'error').mockImplementation();

      const result = await service.getUserBots();

      expect(result.success).toBe(false);
      expect(result.error).toContain('Failed to fetch user bots');
      expect(consoleSpy).toHaveBeenCalledWith(
        'Error fetching user bots:',
        expect.any(Error),
      );

      consoleSpy.mockRestore();
    });

    it('should return user bots with undefined customBot when fetch fails', async () => {
      mockUserBotsRepository.findByUserId.mockResolvedValue(
        Ok([mockUserBot as any]),
      );
      mockCustomBotService.getAllGlobalVersions.mockResolvedValue(
        Failure('Bot not found'),
      );

      const result = await service.getUserBots();

      expect(result.success).toBe(true);
      expect(result.data).toEqual([
        {
          id: 'userbot_123',
          customBotName: 'test-bot',
          acquiredAt: new Date('2023-01-01'),
          customBot: undefined,
        },
      ]);
      expect(mockCustomBotService.getAllGlobalVersions).toHaveBeenCalledWith(
        'test-bot',
      );
    });
  });

  describe('install', () => {
    it('should install a free bot successfully', async () => {
      mockCustomBotService.getAllGlobalVersions.mockResolvedValue(
        Ok(mockCustomBotVersions),
      );
      mockUserBotsRepository.hasUserBot.mockResolvedValue(Ok(false));
      mockUserBotsRepository.addBotToUser.mockResolvedValue(
        Ok(mockUserBot as any),
      );
      // Removed incrementBotInstalls - marketplace functionality not in OSS version

      const result = await service.install('test-bot');

      expect(result.success).toBe(true);
      expect(result.data).toEqual({
        id: 'userbot_123',
        customBotName: 'test-bot',
        acquiredAt: new Date('2023-01-01'),
      });
      expect(mockCustomBotService.getAllGlobalVersions).toHaveBeenCalledWith(
        'test-bot',
      );
      expect(mockUserBotsRepository.hasUserBot).toHaveBeenCalledWith(
        'current_user_123',
        'test-bot',
      );
      expect(mockUserBotsRepository.addBotToUser).toHaveBeenCalledWith(
        'current_user_123',
        'test-bot',
      );
      // Removed incrementBotInstalls expectation - marketplace functionality not in OSS version
    });

    it('should fail when bot is not found', async () => {
      mockCustomBotService.getAllGlobalVersions.mockResolvedValue(
        Failure('Bot not found'),
      );

      const result = await service.install('non-existent-bot');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Failed to fetch bot information');
    });

    it('should fail when bot has no published versions', async () => {
      mockCustomBotService.getAllGlobalVersions.mockResolvedValue(
        Ok({ ...mockCustomBotVersions, versions: [] }),
      );

      const result = await service.install('test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Bot not found or not published');
    });

    it('should fail when bot marketplace information is not available', async () => {
      const botWithoutMarketplace = {
        ...mockCustomBotVersions,
        versions: [
          {
            id: 'version_123',
            status: 'published',
            marketplace: null,
          },
        ],
      } as CustomBotWithVersions;

      mockCustomBotService.getAllGlobalVersions.mockResolvedValue(
        Ok(botWithoutMarketplace),
      );

      const result = await service.install('test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Bot marketplace information not available');
    });

    it('should fail when bot is not free', async () => {
      const paidBot = {
        ...mockCustomBotVersions,
        versions: [
          {
            id: 'version_123',
            status: 'published',
            marketplace: {
              price: 1000,
              isPublished: true,
            },
          },
        ],
      } as CustomBotWithVersions;

      mockCustomBotService.getAllGlobalVersions.mockResolvedValue(Ok(paidBot));

      const result = await service.install('paid-bot');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Bot is not free');
    });

    it('should fail when user already owns the bot', async () => {
      mockCustomBotService.getAllGlobalVersions.mockResolvedValue(
        Ok(mockCustomBotVersions),
      );
      mockUserBotsRepository.hasUserBot.mockResolvedValue(Ok(true));

      const result = await service.install('test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toBe('You already own this bot');
    });

    it('should fail when ownership check fails', async () => {
      mockCustomBotService.getAllGlobalVersions.mockResolvedValue(
        Ok(mockCustomBotVersions),
      );
      mockUserBotsRepository.hasUserBot.mockResolvedValue(
        Failure('Database error'),
      );

      const result = await service.install('test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Failed to check bot ownership');
    });

    it('should fail when adding bot to user collection fails', async () => {
      mockCustomBotService.getAllGlobalVersions.mockResolvedValue(
        Ok(mockCustomBotVersions),
      );
      mockUserBotsRepository.hasUserBot.mockResolvedValue(Ok(false));
      mockUserBotsRepository.addBotToUser.mockResolvedValue(
        Failure('Database error'),
      );

      const result = await service.install('test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toBe(
        'Failed to add bot to collection: Database error',
      );
    });

    it('should handle unexpected errors', async () => {
      mockCustomBotService.getAllGlobalVersions.mockRejectedValue(
        new Error('Unexpected error'),
      );

      const consoleSpy = jest.spyOn(console, 'error').mockImplementation();

      const result = await service.install('test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toContain('Failed to purchase free bot');
      expect(consoleSpy).toHaveBeenCalledWith(
        'Error purchasing free bot:',
        expect.any(Error),
      );

      consoleSpy.mockRestore();
    });
  });

  describe('hasUserBot', () => {
    it('should return true when user has the bot', async () => {
      mockUserBotsRepository.hasUserBot.mockResolvedValue(Ok(true));

      const result = await service.hasUserBot('user_123', 'test-bot');

      expect(result.success).toBe(true);
      expect(result.data).toBe(true);
      expect(mockUserBotsRepository.hasUserBot).toHaveBeenCalledWith(
        'user_123',
        'test-bot',
      );
    });

    it('should return false when user does not have the bot', async () => {
      mockUserBotsRepository.hasUserBot.mockResolvedValue(Ok(false));

      const result = await service.hasUserBot('user_123', 'test-bot');

      expect(result.success).toBe(true);
      expect(result.data).toBe(false);
    });

    it('should handle repository errors', async () => {
      mockUserBotsRepository.hasUserBot.mockResolvedValue(
        Failure('Database error'),
      );

      const result = await service.hasUserBot('user_123', 'test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Database error');
    });

    it('should handle unexpected errors', async () => {
      mockUserBotsRepository.hasUserBot.mockRejectedValue(
        new Error('Unexpected error'),
      );

      const consoleSpy = jest.spyOn(console, 'error').mockImplementation();

      const result = await service.hasUserBot('user_123', 'test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toContain('Failed to check bot ownership');
      expect(consoleSpy).toHaveBeenCalledWith(
        'Error checking user bot ownership:',
        expect.any(Error),
      );

      consoleSpy.mockRestore();
    });
  });

  describe('getCurrentUserHasBot', () => {
    it('should check if current user has the bot', async () => {
      mockUserBotsRepository.hasUserBot.mockResolvedValue(Ok(true));

      const result = await service.getCurrentUserHasBot('test-bot');

      expect(result.success).toBe(true);
      expect(result.data).toBe(true);
      expect(mockUserBotsRepository.hasUserBot).toHaveBeenCalledWith(
        'current_user_123',
        'test-bot',
      );
    });

    it('should handle repository errors', async () => {
      mockUserBotsRepository.hasUserBot.mockResolvedValue(
        Failure('Database error'),
      );

      const result = await service.getCurrentUserHasBot('test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Database error');
    });

    it('should handle unexpected errors', async () => {
      mockUserBotsRepository.hasUserBot.mockRejectedValue(
        new Error('Unexpected error'),
      );

      const consoleSpy = jest.spyOn(console, 'error').mockImplementation();

      const result = await service.getCurrentUserHasBot('test-bot');

      expect(result.success).toBe(false);
      expect(result.error).toContain('Failed to check bot ownership');
      expect(consoleSpy).toHaveBeenCalledWith(
        'Error checking user bot ownership:',
        expect.any(Error),
      );

      consoleSpy.mockRestore();
    });
  });

  describe('data transformation', () => {
    it('should transform user bot data correctly', async () => {
      const multipleUserBots = [
        {
          id: 'userbot_123',
          customBotName: 'test-bot',
          acquiredAt: new Date('2023-01-01'),
        },
        {
          id: 'userbot_456',
          customBotName: 'another-bot',
          acquiredAt: new Date('2023-01-02'),
        },
      ];

      mockUserBotsRepository.findByUserId.mockResolvedValue(
        Ok(multipleUserBots as any),
      );
      mockCustomBotService.getAllGlobalVersions.mockResolvedValue(
        Ok(mockCustomBotVersions),
      );

      const result = await service.getUserBots();

      expect(result.success).toBe(true);
      expect(result.data).toHaveLength(2);
      expect(result.data[0]).toEqual({
        id: 'userbot_123',
        customBotName: 'test-bot',
        acquiredAt: new Date('2023-01-01'),
        customBot: mockCustomBotVersions,
      });
      expect(result.data[1]).toEqual({
        id: 'userbot_456',
        customBotName: 'another-bot',
        acquiredAt: new Date('2023-01-02'),
        customBot: mockCustomBotVersions,
      });
      expect(mockCustomBotService.getAllGlobalVersions).toHaveBeenCalledTimes(
        2,
      );
      expect(mockCustomBotService.getAllGlobalVersions).toHaveBeenCalledWith(
        'test-bot',
      );
      expect(mockCustomBotService.getAllGlobalVersions).toHaveBeenCalledWith(
        'another-bot',
      );
    });
  });
});
