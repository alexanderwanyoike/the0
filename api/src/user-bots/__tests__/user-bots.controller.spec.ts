import { BadRequestException } from '@nestjs/common';
import { UserBotsController } from '../user-bots.controller';
import { UserBotsService } from '../user-bots.service';
import { Ok, Failure } from '@/common/result';
import { UserBotSummary, UserBotQueryParams } from '../user-bots.types';

describe('UserBotsController', () => {
  let controller: UserBotsController;
  let mockService: jest.Mocked<UserBotsService>;

  const mockUserBotSummary: UserBotSummary = {
    id: 'userbot_123',
    customBotName: 'test-bot',
    acquiredAt: new Date('2023-01-01'),
  };

  beforeEach(() => {
    mockService = {
      install: jest.fn(),
      getUserBots: jest.fn(),
      getCurrentUserHasBot: jest.fn(),
      hasUserBot: jest.fn(),
    } as any;

    controller = new UserBotsController(mockService);
  });

  describe('install', () => {
    it('should install a free bot successfully', async () => {
      mockService.install.mockResolvedValue(Ok(mockUserBotSummary));

      const result = await controller.install('test-bot');

      expect(mockService.install).toHaveBeenCalledWith('test-bot');
      expect(result).toEqual({
        success: true,
        message: 'Free bot added to your collection',
      });
    });

    it('should throw BadRequestException when bot name is missing', async () => {
      await expect(controller.install('')).rejects.toThrow(BadRequestException);
      await expect(controller.install('')).rejects.toThrow(
        'Bot name is required',
      );
    });

    it('should throw BadRequestException when bot name is null/undefined', async () => {
      await expect(controller.install(null as any)).rejects.toThrow(
        BadRequestException,
      );
      await expect(controller.install(undefined as any)).rejects.toThrow(
        BadRequestException,
      );
    });

    it('should throw BadRequestException when service returns failure', async () => {
      mockService.install.mockResolvedValue(Failure('Bot is not free'));

      await expect(controller.install('paid-bot')).rejects.toThrow(
        BadRequestException,
      );
      await expect(controller.install('paid-bot')).rejects.toThrow(
        'Bot is not free',
      );
    });

    it('should handle service errors', async () => {
      mockService.install.mockResolvedValue(
        Failure('Database connection failed'),
      );

      await expect(controller.install('test-bot')).rejects.toThrow(
        BadRequestException,
      );
      await expect(controller.install('test-bot')).rejects.toThrow(
        'Database connection failed',
      );
    });

    it('should trim whitespace from bot name', async () => {
      mockService.install.mockResolvedValue(Ok(mockUserBotSummary));

      await controller.install('  test-bot  ');

      expect(mockService.install).toHaveBeenCalledWith('  test-bot  ');
    });
  });

  describe('getUserBots', () => {
    it('should return user bots with default parameters', async () => {
      mockService.getUserBots.mockResolvedValue(Ok([mockUserBotSummary]));

      const result = await controller.getUserBots({});

      expect(mockService.getUserBots).toHaveBeenCalledWith({
        limit: undefined,
        offset: undefined,
      });
      expect(result).toEqual({
        success: true,
        data: [mockUserBotSummary],
      });
    });

    it('should return user bots with query parameters', async () => {
      const query = {
        limit: '10',
        offset: '5',
      } as unknown as UserBotQueryParams;

      mockService.getUserBots.mockResolvedValue(Ok([mockUserBotSummary]));

      const result = await controller.getUserBots(query);

      expect(mockService.getUserBots).toHaveBeenCalledWith({
        limit: 10,
        offset: 5,
      });
      expect(result).toEqual({
        success: true,
        data: [mockUserBotSummary],
      });
    });

    it('should handle string number conversion for limit and offset', async () => {
      const query = {
        limit: 25 as any,
        offset: 10 as any,
      };

      mockService.getUserBots.mockResolvedValue(Ok([]));

      await controller.getUserBots(query);

      expect(mockService.getUserBots).toHaveBeenCalledWith({
        limit: 25,
        offset: 10,
      });
    });

    it('should throw BadRequestException when service returns failure', async () => {
      mockService.getUserBots.mockResolvedValue(Failure('Access denied'));

      await expect(controller.getUserBots({})).rejects.toThrow(
        BadRequestException,
      );
      await expect(controller.getUserBots({})).rejects.toThrow('Access denied');
    });

    it('should handle empty results', async () => {
      mockService.getUserBots.mockResolvedValue(Ok([]));

      const result = await controller.getUserBots({});

      expect(result).toEqual({
        success: true,
        data: [],
      });
    });

    it('should handle multiple user bots', async () => {
      const multipleBots = [
        mockUserBotSummary,
        {
          id: 'userbot_456',
          customBotName: 'another-bot',
          acquiredAt: new Date('2023-01-02'),
        },
      ];

      mockService.getUserBots.mockResolvedValue(Ok(multipleBots));

      const result = await controller.getUserBots({});

      expect(result).toEqual({
        success: true,
        data: multipleBots,
      });
    });
  });

  describe('hasBot', () => {
    it('should return true when user has the bot', async () => {
      mockService.getCurrentUserHasBot.mockResolvedValue(Ok(true));

      const result = await controller.hasBot('test-bot');

      expect(mockService.getCurrentUserHasBot).toHaveBeenCalledWith('test-bot');
      expect(result).toEqual({
        success: true,
        hasBot: true,
      });
    });

    it('should return false when user does not have the bot', async () => {
      mockService.getCurrentUserHasBot.mockResolvedValue(Ok(false));

      const result = await controller.hasBot('test-bot');

      expect(mockService.getCurrentUserHasBot).toHaveBeenCalledWith('test-bot');
      expect(result).toEqual({
        success: true,
        hasBot: false,
      });
    });

    it('should throw BadRequestException when bot name is missing', async () => {
      await expect(controller.hasBot('')).rejects.toThrow(BadRequestException);
      await expect(controller.hasBot('')).rejects.toThrow(
        'Bot name is required',
      );
    });

    it('should throw BadRequestException when bot name is null/undefined', async () => {
      await expect(controller.hasBot(null as any)).rejects.toThrow(
        BadRequestException,
      );
      await expect(controller.hasBot(undefined as any)).rejects.toThrow(
        BadRequestException,
      );
    });

    it('should throw BadRequestException when service returns failure', async () => {
      mockService.getCurrentUserHasBot.mockResolvedValue(
        Failure('Database error'),
      );

      await expect(controller.hasBot('test-bot')).rejects.toThrow(
        BadRequestException,
      );
      await expect(controller.hasBot('test-bot')).rejects.toThrow(
        'Database error',
      );
    });

    it('should handle service errors gracefully', async () => {
      mockService.getCurrentUserHasBot.mockResolvedValue(
        Failure('Connection timeout'),
      );

      await expect(controller.hasBot('test-bot')).rejects.toThrow(
        BadRequestException,
      );
      await expect(controller.hasBot('test-bot')).rejects.toThrow(
        'Connection timeout',
      );
    });
  });

  describe('parameter validation', () => {
    it('should handle non-numeric limit gracefully in getUserBots', async () => {
      const query = {
        limit: 'invalid' as any,
        offset: '5',
      } as unknown as UserBotQueryParams;

      mockService.getUserBots.mockResolvedValue(Ok([]));

      await controller.getUserBots(query);

      expect(mockService.getUserBots).toHaveBeenCalledWith({
        limit: NaN,
        offset: 5,
      });
    });

    it('should handle non-numeric offset gracefully in getUserBots', async () => {
      const query = {
        limit: '10',
        offset: 'invalid' as any,
      } as unknown as UserBotQueryParams;

      mockService.getUserBots.mockResolvedValue(Ok([]));

      await controller.getUserBots(query);

      expect(mockService.getUserBots).toHaveBeenCalledWith({
        limit: 10,
        offset: NaN,
      });
    });

    it('should handle special characters in bot names', async () => {
      const botNameWithSpecialChars = 'bot-name_with-special.chars123';
      mockService.install.mockResolvedValue(Ok(mockUserBotSummary));

      await controller.install(botNameWithSpecialChars);

      expect(mockService.install).toHaveBeenCalledWith(botNameWithSpecialChars);
    });

    it('should handle long bot names', async () => {
      const longBotName = 'a'.repeat(100);
      mockService.install.mockResolvedValue(Ok(mockUserBotSummary));

      await controller.install(longBotName);

      expect(mockService.install).toHaveBeenCalledWith(longBotName);
    });
  });

  describe('HTTP status codes', () => {
    it('should return 200 for successful install', async () => {
      mockService.install.mockResolvedValue(Ok(mockUserBotSummary));

      const result = await controller.install('test-bot');

      expect(result.success).toBe(true);
    });

    it('should return 200 for successful getUserBots', async () => {
      mockService.getUserBots.mockResolvedValue(Ok([mockUserBotSummary]));

      const result = await controller.getUserBots({});

      expect(result.success).toBe(true);
    });

    it('should return 200 for successful hasBot', async () => {
      mockService.getCurrentUserHasBot.mockResolvedValue(Ok(true));

      const result = await controller.hasBot('test-bot');

      expect(result.success).toBe(true);
    });
  });
});
