import { CustomBotService } from '../custom-bot.service';
import { CustomBotRepository } from '../custom-bot.repository';
import { StorageService } from '../storage.service';
import {
  CustomBot,
  CustomBotConfig,
  CustomBotWithVersions,
} from '../custom-bot.types';
import { Ok, Failure } from '@/common/result';
import { validateCustomBotConfigPayload } from '../custom-bot.schema';
// Removed StripeConnectService - not needed in OSS version

// Mock dependencies
jest.mock('../custom-bot.repository');
jest.mock('../storage.service');
jest.mock('../custom-bot.schema', () => ({
  validateCustomBotConfigPayload: jest.fn(),
}));

describe('CustomBotService', () => {
  let service: CustomBotService;
  let mockRepository: jest.Mocked<CustomBotRepository>;
  let mockStorageService: jest.Mocked<StorageService>;
  // Removed StripeConnectService mock - not needed in OSS version
  let mockValidateConfig: jest.MockedFunction<
    typeof validateCustomBotConfigPayload
  >;

  const validConfig: CustomBotConfig = {
    name: 'test-bot',
    description: 'Test bot description',
    version: '1.0.0',
    author: 'Test Author',
    type: 'scheduled',
    runtime: 'python3.11',
    entrypoints: {
      bot: 'main.py',
      backtest: 'backtest.py',
    },
    schema: {
      bot: { type: 'object' },
      backtest: { type: 'object' },
    },
    readme:
      'This is a test bot with enough content to pass validation requirements for the readme field.',
  };

  const createMockFile = (
    originalname = 'test-bot.zip',
    mimetype = 'application/zip',
    size = 1024,
    buffer: Buffer = Buffer.from('test zip content'),
  ): Express.Multer.File => ({
    fieldname: 'file',
    originalname,
    encoding: '7bit',
    mimetype,
    size,
    buffer,
    destination: '',
    filename: '',
    path: '',
    stream: null as any,
  });

  beforeEach(() => {
    mockRepository =
      new CustomBotRepository() as jest.Mocked<CustomBotRepository>;
    mockStorageService = new StorageService() as jest.Mocked<StorageService>;
    mockValidateConfig = validateCustomBotConfigPayload as jest.MockedFunction<
      typeof validateCustomBotConfigPayload
    >;

    service = new CustomBotService(
      mockRepository,
      mockStorageService,
    );
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  describe('createCustomBot', () => {
    it('should create a custom bot successfully', async () => {
      const userId = 'user123';
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      // Setup mocks
      mockValidateConfig.mockReturnValue({ valid: true });
      mockRepository.globalBotExists.mockResolvedValue(Ok(false));
      mockStorageService.downloadAndValidateZipStructure.mockResolvedValue(
        Ok(true),
      );
      const mockResult = {
        id: 'new-bot-id',
        name: 'test-bot',
        version: '1.0.0',
        config: validConfig,
        status: 'pending_review',
        filePath:
          'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip',
        userId,
        createdAt: new Date(),
        updatedAt: new Date(),
      } as CustomBot;
      mockRepository.createNewGlobalVersion.mockResolvedValue(Ok(mockResult));
      mockRepository.getSpecificGlobalVersion.mockResolvedValue(Ok(mockResult));

      const result = await service.createCustomBot(
        userId,
        validConfig,
        filePath,
      );

      expect(result.success).toBe(true);
      expect(result.data).toBeDefined();
      expect(result.data!.name).toBe('test-bot');
      expect(mockRepository.globalBotExists).toHaveBeenCalledWith('test-bot');
      expect(
        mockStorageService.downloadAndValidateZipStructure,
      ).toHaveBeenCalledWith(filePath, Object.values(validConfig.entrypoints).filter(Boolean));
    });

    it('should fail validation with invalid config', async () => {
      const userId = 'user123';
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({
        valid: false,
        errors: ['name is required', 'version format invalid'],
      });

      const result = await service.createCustomBot(
        userId,
        validConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toContain('Validation failed');
      expect(result.error).toContain('name is required');
    });

    it('should fail when bot already exists', async () => {
      const userId = 'user123';
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });
      mockRepository.globalBotExists.mockResolvedValue(Ok(true));

      const result = await service.createCustomBot(
        userId,
        validConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toBe('Custom bot with this name already exists');
    });

    it('should fail when ZIP validation fails', async () => {
      const userId = 'user123';
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });
      mockRepository.globalBotExists.mockResolvedValue(Ok(false));
      mockStorageService.downloadAndValidateZipStructure.mockResolvedValue(
        Failure('Required entrypoint not found'),
      );

      const result = await service.createCustomBot(
        userId,
        validConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toContain('ZIP validation failed');
    });

    it('should fail when ZIP validation fails', async () => {
      const userId = 'user123';
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });
      mockRepository.globalBotExists.mockResolvedValue(Ok(false));
      mockStorageService.downloadAndValidateZipStructure.mockResolvedValue(
        Failure('Required entrypoint not found'),
      );

      const result = await service.createCustomBot(
        userId,
        validConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toContain('ZIP validation failed');
    });

    it('should handle repository errors', async () => {
      const userId = 'user123';
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });
      mockRepository.globalBotExists.mockResolvedValue(Ok(false));
      mockStorageService.downloadAndValidateZipStructure.mockResolvedValue(
        Ok(true),
      );
      mockRepository.createNewGlobalVersion.mockResolvedValue(
        Failure('Database error'),
      );

      const result = await service.createCustomBot(
        userId,
        validConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toBe('Database error');
    });

    it('should handle unexpected errors gracefully', async () => {
      const userId = 'user123';
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });
      mockRepository.globalBotExists.mockRejectedValue(
        new Error('Unexpected error'),
      );

      const result = await service.createCustomBot(
        userId,
        validConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toContain('Failed to create custom bot');
      expect(result.error).toContain('Unexpected error');
    });

    it('should fail when realtime bot does not specify runtime', async () => {
      const userId = 'user123';
      const realtimeConfig = {
        ...validConfig,
        type: 'realtime' as const,
        runtime: undefined as any, // Realtime bot without runtime
      };
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });
      mockRepository.globalBotExists.mockResolvedValue(Ok(false));
      mockStorageService.downloadAndValidateZipStructure.mockResolvedValue(
        Ok(true),
      );

      const mockResult = {
        id: 'new-bot-id',
        name: 'test-bot',
        version: '1.0.0',
        config: validConfig,
        status: 'pending_review',
        filePath:
          'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip',
        userId,
        createdAt: new Date(),
        updatedAt: new Date(),
      } as CustomBot;
      mockRepository.createNewGlobalVersion.mockResolvedValue(Ok(mockResult));
      mockRepository.getSpecificGlobalVersion.mockResolvedValue(Ok(mockResult));

      const result = await service.createCustomBot(
        userId,
        realtimeConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toBe(
        'Realtime bots must specify a valid runtime (python3.11 or nodejs20)',
      );
      expect(
        mockStorageService.downloadAndValidateZipStructure,
      ).not.toHaveBeenCalled();
    });

    it('should fail when scheduled bot uses nodejs20 runtime', async () => {
      const userId = 'user123';
      const scheduledNodeConfig = {
        ...validConfig,
        type: 'scheduled' as const,
        runtime: 'nodejs20' as const, // Invalid for scheduled bots
      };
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });
      mockRepository.globalBotExists.mockResolvedValue(Ok(false));

      const result = await service.createCustomBot(
        userId,
        scheduledNodeConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toBe(
        'Scheduled bots must use python3.11 runtime (nodejs20 is not supported for scheduled bots)',
      );
      expect(
        mockStorageService.downloadAndValidateZipStructure,
      ).not.toHaveBeenCalled();
    });
  });

  describe('updateCustomBot', () => {
    it('should update custom bot with new version successfully', async () => {
      const userId = 'user123';
      const botName = 'test-bot';
      const updateConfig = {
        ...validConfig,
        version: '1.1.0',
      };
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });
      mockRepository.globalBotExists.mockResolvedValue(Ok(true));
      mockRepository.checkUserOwnership.mockResolvedValue(Ok(true));
      mockRepository.getGlobalLatestVersion.mockResolvedValue(
        Ok({
          id: 'existing-id',
          name: 'test-bot',
          version: '1.0.0',
          config: validConfig,
          filePath: 'gs://old-path.zip',
          status: 'approved',
          userId,
          createdAt: new Date(),
          updatedAt: new Date(),
        }),
      );
      mockRepository.isVersionNewer.mockReturnValue(true);
      mockRepository.globalVersionExists.mockResolvedValue(Ok(false));
      mockStorageService.downloadAndValidateZipStructure.mockResolvedValue(
        Ok(true),
      );
      mockRepository.createNewGlobalVersion.mockResolvedValue(
        Ok({
          id: 'new-version-id',
          name: 'test-bot',
          version: '1.1.0',
          config: updateConfig,
          status: 'pending_review',
          filePath:
            'gs://test-bucket/user123/test-bot/1.1.0/test-bot_1.1.0_123456.zip',
          userId,
          createdAt: new Date(),
          updatedAt: new Date(),
        }),
      );

      const result = await service.updateCustomBot(
        userId,
        botName,
        updateConfig,
        filePath,
      );

      expect(result.success).toBe(true);
      expect(result.data!.version).toBe('1.1.0');
    });

    it('should fail when the realtime bot does not specify runtime', async () => {
      const userId = 'user123';
      const botName = 'test-bot';
      const realtimeConfig = {
        ...validConfig,
        name: botName,
        type: 'realtime' as const,
        runtime: undefined as any, // Invalid runtime for realtime bot
      };
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });

      const result = await service.updateCustomBot(
        userId,
        botName,
        realtimeConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toBe(
        'Realtime bots must specify a valid runtime (python3.11 or nodejs20)',
      );
      expect(
        mockStorageService.downloadAndValidateZipStructure,
      ).not.toHaveBeenCalled();
    });

    it('should fail when scheduled bot uses nodejs20 runtime', async () => {
      const userId = 'user123';
      const botName = 'test-bot';
      const scheduledNodeConfig = {
        ...validConfig,
        name: botName,
        type: 'scheduled' as const,
        runtime: 'nodejs20' as const, // Invalid for scheduled bots
      };
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });

      const result = await service.updateCustomBot(
        userId,
        botName,
        scheduledNodeConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toBe(
        'Scheduled bots must use python3.11 runtime (nodejs20 is not supported for scheduled bots)',
      );
      expect(
        mockStorageService.downloadAndValidateZipStructure,
      ).not.toHaveBeenCalled();
    });

    it('should fail when bot name in config does not match URL parameter', async () => {
      const userId = 'user123';
      const botName = 'different-bot';
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });

      const result = await service.updateCustomBot(
        userId,
        botName,
        validConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toBe(
        'Bot name in config must match the URL parameter',
      );
    });

    it('should fail when bot does not exist', async () => {
      const userId = 'user123';
      const botName = 'test-bot';
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });
      mockRepository.globalBotExists.mockResolvedValue(Ok(false));

      const result = await service.updateCustomBot(
        userId,
        botName,
        validConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toBe(
        'Custom bot does not exist. Create it first using POST.',
      );
    });

    it('should fail when user does not own the bot', async () => {
      const userId = 'user123';
      const botName = 'test-bot';
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });
      mockRepository.globalBotExists.mockResolvedValue(Ok(true));
      mockRepository.checkUserOwnership.mockResolvedValue(
        Failure('Insufficient permissions'),
      );

      const result = await service.updateCustomBot(
        userId,
        botName,
        validConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toBe('Insufficient permissions');
    });

    it('should fail when new version is not newer', async () => {
      const userId = 'user123';
      const botName = 'test-bot';
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });
      mockRepository.globalBotExists.mockResolvedValue(Ok(true));
      mockRepository.checkUserOwnership.mockResolvedValue(Ok(true));
      mockRepository.getGlobalLatestVersion.mockResolvedValue(
        Ok({
          id: 'existing-id',
          name: 'test-bot',
          version: '2.0.0', // Higher than payload version
          config: validConfig,
          status: 'approved',
          filePath: 'gs://old-path.zip',
          userId,
          createdAt: new Date(),
          updatedAt: new Date(),
        }),
      );
      mockRepository.isVersionNewer.mockReturnValue(false);

      const result = await service.updateCustomBot(
        userId,
        botName,
        validConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toContain('must be greater than current version');
    });

    it('should fail when version already exists', async () => {
      const userId = 'user123';
      const botName = 'test-bot';
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });
      mockRepository.globalBotExists.mockResolvedValue(Ok(true));
      mockRepository.checkUserOwnership.mockResolvedValue(Ok(true));
      mockRepository.getGlobalLatestVersion.mockResolvedValue(
        Ok({
          id: 'existing-id',
          name: 'test-bot',
          version: '0.9.0',
          config: validConfig,
          status: 'approved',
          filePath: 'gs://old-path.zip',
          userId,
          createdAt: new Date(),
          updatedAt: new Date(),
        }),
      );
      mockRepository.isVersionNewer.mockReturnValue(true);
      mockRepository.globalVersionExists.mockResolvedValue(Ok(true));

      const result = await service.updateCustomBot(
        userId,
        botName,
        validConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toContain('already exists for this bot');
    });

    it('should fail when ZIP validation fails', async () => {
      const userId = 'user123';
      const botName = 'test-bot';
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });
      mockRepository.globalBotExists.mockResolvedValue(Ok(true));
      mockRepository.checkUserOwnership.mockResolvedValue(Ok(true));
      mockRepository.getGlobalLatestVersion.mockResolvedValue(
        Ok({
          id: 'existing-id',
          name: 'test-bot',
          version: '0.9.0',
          config: validConfig,
          status: 'approved',
          filePath: 'gs://old-path.zip',
          userId,
          createdAt: new Date(),
          updatedAt: new Date(),
        }),
      );
      mockRepository.isVersionNewer.mockReturnValue(true);
      mockRepository.globalVersionExists.mockResolvedValue(Ok(false));
      mockStorageService.downloadAndValidateZipStructure.mockResolvedValue(
        Failure('Invalid ZIP structure'),
      );

      const result = await service.updateCustomBot(
        userId,
        botName,
        validConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toContain('ZIP validation failed');
    });

    it('should handle unexpected errors gracefully', async () => {
      const userId = 'user123';
      const botName = 'test-bot';
      const filePath =
        'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip';

      mockValidateConfig.mockReturnValue({ valid: true });
      mockRepository.globalBotExists.mockRejectedValue(
        new Error('Database connection failed'),
      );

      const result = await service.updateCustomBot(
        userId,
        botName,
        validConfig,
        filePath,
      );

      expect(result.success).toBe(false);
      expect(result.error).toContain('Failed to update custom bot');
      expect(result.error).toContain('Database connection failed');
    });
  });

  describe('getUserCustomBots', () => {
    it('should return user custom bots successfully', async () => {
      const userId = 'user123';
      const mockBotsWithVersions: CustomBotWithVersions[] = [
        {
          id: 'bot-1',
          name: 'arbitrage-bot',
          userId,
          latestVersion: '1.2.0',
          versions: [
            {
              version: '1.2.0',
              config: {
                ...validConfig,
                name: 'arbitrage-bot',
                version: '1.2.0',
              },
              marketplace: null,
              status: 'approved',
              id: 'bot-1',
              userId,
              filePath: 'gs://bucket/arbitrage-bot/1.2.0/file.zip',
              createdAt: new Date('2025-01-15T10:30:00.000Z'),
              updatedAt: new Date('2025-01-15T10:30:00.000Z'),
            },
            {
              version: '1.1.0',
              config: {
                ...validConfig,
                name: 'arbitrage-bot',
                version: '1.1.0',
              },
              marketplace: null,
              status: 'approved',
              id: 'bot-1',
              userId,
              filePath: 'gs://bucket/arbitrage-bot/1.1.0/file.zip',
              createdAt: new Date('2025-01-10T09:15:00.000Z'),
              updatedAt: new Date('2025-01-10T09:15:00.000Z'),
            },
          ],
          createdAt: new Date('2025-01-10T09:15:00.000Z'),
          updatedAt: new Date('2025-01-15T10:30:00.000Z'),
        },
        {
          id: 'bot-2',
          name: 'trend-following-bot',
          userId,
          latestVersion: '2.0.0',
          versions: [
            {
              version: '2.0.0',
              config: {
                ...validConfig,
                name: 'trend-following-bot',
                version: '2.0.0',
              },
              marketplace: null,
              status: 'approved',
              id: 'bot-1',
              userId,
              filePath: 'gs://bucket/trend-following-bot/2.0.0/file.zip',
              createdAt: new Date('2025-01-12T14:20:00.000Z'),
              updatedAt: new Date('2025-01-12T14:20:00.000Z'),
            },
          ],
          createdAt: new Date('2025-01-12T14:20:00.000Z'),
          updatedAt: new Date('2025-01-12T14:20:00.000Z'),
        },
      ];

      mockRepository.getUserCustomBots.mockResolvedValue(
        Ok(mockBotsWithVersions),
      );

      const result = await service.getUserCustomBots(userId);

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockBotsWithVersions);
      expect(result.data!.length).toBe(2);
      expect(result.data![0].name).toBe('arbitrage-bot');
      expect(result.data![0].versions.length).toBe(2);
      expect(result.data![1].name).toBe('trend-following-bot');
      expect(result.data![1].versions.length).toBe(1);
      expect(mockRepository.getUserCustomBots).toHaveBeenCalledWith(userId);
    });

    it('should return empty array when user has no custom bots', async () => {
      const userId = 'user123';

      mockRepository.getUserCustomBots.mockResolvedValue(Ok([]));

      const result = await service.getUserCustomBots(userId);

      expect(result.success).toBe(true);
      expect(result.data).toEqual([]);
      expect(mockRepository.getUserCustomBots).toHaveBeenCalledWith(userId);
    });

    it('should handle repository errors', async () => {
      const userId = 'user123';

      mockRepository.getUserCustomBots.mockResolvedValue(
        Failure('Database connection failed'),
      );

      const result = await service.getUserCustomBots(userId);

      expect(result.success).toBe(false);
      expect(result.error).toBe('Database connection failed');
      expect(mockRepository.getUserCustomBots).toHaveBeenCalledWith(userId);
    });

    it('should handle unexpected errors gracefully', async () => {
      const userId = 'user123';

      mockRepository.getUserCustomBots.mockRejectedValue(
        new Error('Network timeout'),
      );

      const result = await service.getUserCustomBots(userId);

      expect(result.success).toBe(false);
      expect(result.error).toContain('Failed to get user custom bots');
      expect(result.error).toContain('Network timeout');
    });

    it('should maintain correct data structure for grouped bots', async () => {
      const userId = 'user123';
      const mockBotWithMultipleVersions: CustomBotWithVersions[] = [
        {
          id: 'bot-complex',
          name: 'multi-version-bot',
          userId,
          latestVersion: '3.0.0',
          versions: [
            {
              version: '3.0.0',
              config: {
                ...validConfig,
                name: 'multi-version-bot',
                version: '3.0.0',
              },
              marketplace: null,
              status: 'approved',
              id: 'bot-1',
              userId,
              filePath: 'gs://bucket/multi-version-bot/3.0.0/file.zip',
              createdAt: new Date('2025-01-15T10:30:00.000Z'),
              updatedAt: new Date('2025-01-15T10:30:00.000Z'),
            },
            {
              version: '2.1.0',
              config: {
                ...validConfig,
                name: 'multi-version-bot',
                version: '2.1.0',
              },
              marketplace: null,
              status: 'approved',
              id: 'bot-1',
              userId,
              filePath: 'gs://bucket/multi-version-bot/2.1.0/file.zip',
              createdAt: new Date('2025-01-12T10:30:00.000Z'),
              updatedAt: new Date('2025-01-12T10:30:00.000Z'),
            },
            {
              version: '1.0.0',
              config: {
                ...validConfig,
                name: 'multi-version-bot',
                version: '1.0.0',
              },
              marketplace: null,
              status: 'approved',
              id: 'bot-1',
              userId,
              filePath: 'gs://bucket/multi-version-bot/1.0.0/file.zip',
              createdAt: new Date('2025-01-10T10:30:00.000Z'),
              updatedAt: new Date('2025-01-10T10:30:00.000Z'),
            },
          ],
          createdAt: new Date('2025-01-10T10:30:00.000Z'), // First created
          updatedAt: new Date('2025-01-15T10:30:00.000Z'), // Latest updated
        },
      ];

      mockRepository.getUserCustomBots.mockResolvedValue(
        Ok(mockBotWithMultipleVersions),
      );

      const result = await service.getUserCustomBots(userId);

      expect(result.success).toBe(true);
      expect(result.data![0].latestVersion).toBe('3.0.0');
      expect(result.data![0].versions).toHaveLength(3);
      expect(result.data![0].versions[0].version).toBe('3.0.0'); // Latest first
      expect(result.data![0].versions[2].version).toBe('1.0.0'); // Oldest last
    });
  });

  describe('getAllUserVersions', () => {
    it('should return all versions for a specific bot successfully', async () => {
      const userId = 'user123';
      const botName = 'test-bot';
      const mockVersions: CustomBotWithVersions = {
        id: 'bot-id',
        name: 'test-bot',
        userId,
        versions: [
          {
            version: '1.1.0',
            config: { ...validConfig, version: '1.1.0' },
            filePath: 'path1',
            createdAt: new Date(),
            updatedAt: new Date(),
            marketplace: null,
            status: 'approved',
            id: 'bot-1',
            userId,
          },
          {
            version: '1.0.0',
            config: validConfig,
            filePath: 'path2',
            createdAt: new Date(),
            updatedAt: new Date(),
            marketplace: null,
            status: 'approved',
            id: 'bot-1',
            userId,
          },
        ],
        latestVersion: '1.1.0',
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      mockRepository.getAllUserVersions.mockResolvedValue(Ok(mockVersions));

      const result = await service.getAllUserVersions(userId, botName);

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockVersions);
      expect(mockRepository.getAllUserVersions).toHaveBeenCalledWith(
        userId,
        botName,
      );
    });

    it('should handle repository errors', async () => {
      const userId = 'user123';
      const botName = 'test-bot';

      mockRepository.getAllUserVersions.mockResolvedValue(
        Failure('Bot not found'),
      );

      const result = await service.getAllUserVersions(userId, botName);

      expect(result.success).toBe(false);
      expect(result.error).toBe('Bot not found');
    });
  });

  describe('getAllGlobalVersions', () => {
    it('should return all global versions for a bot successfully', async () => {
      const botName = 'test-bot';
      const mockVersions: CustomBotWithVersions = {
        id: 'bot-id',
        name: 'test-bot',
        userId: 'any-user',
        versions: [
          {
            version: '2.0.0',
            config: { ...validConfig, version: '2.0.0' },
            filePath: 'path1',
            createdAt: new Date(),
            updatedAt: new Date(),
            marketplace: null,
            status: 'approved',
            id: 'bot-1',
            userId: 'any-user',
          },
        ],
        latestVersion: '2.0.0',
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      mockRepository.getAllGlobalVersions.mockResolvedValue(Ok(mockVersions));

      const result = await service.getAllGlobalVersions(botName);

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockVersions);
      expect(mockRepository.getAllGlobalVersions).toHaveBeenCalledWith(botName);
    });
  });

  describe('getUserSpecificVersion', () => {
    it('should return specific version successfully', async () => {
      const userId = 'user123';
      const botName = 'test-bot';
      const version = '1.0.0';
      const mockBot = {
        id: 'bot-id',
        name: 'test-bot',
        version: '1.0.0',
        config: validConfig,
        status: 'approved',
        filePath:
          'gs://test-bucket/user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip',
        userId,
        createdAt: new Date(),
        updatedAt: new Date(),
      } as CustomBot;

      mockRepository.getSpecificUserVersion.mockResolvedValue(Ok(mockBot));

      const result = await service.getUserSpecificVersion(
        userId,
        botName,
        version,
      );

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockBot);
      expect(mockRepository.getSpecificUserVersion).toHaveBeenCalledWith(
        userId,
        botName,
        version,
      );
    });

    it('should handle version not found', async () => {
      const userId = 'user123';
      const botName = 'test-bot';
      const version = '1.0.0';

      mockRepository.getSpecificUserVersion.mockResolvedValue(
        Failure('Version not found'),
      );

      const result = await service.getUserSpecificVersion(
        userId,
        botName,
        version,
      );

      expect(result.success).toBe(false);
      expect(result.error).toBe('Version not found');
    });
  });

  describe('getGlobalSpecificVersion', () => {
    it('should return global specific version successfully', async () => {
      const botName = 'test-bot';
      const version = '1.0.0';
      const mockBot = {
        id: 'bot-id',
        name: 'test-bot',
        version: '1.0.0',
        config: validConfig,
        filePath: 'gs://test-bucket/test-bot/1.0.0/file.zip',
        userId: 'any-user',
        status: 'approved',
        createdAt: new Date(),
        updatedAt: new Date(),
      } as CustomBot;

      mockRepository.getSpecificGlobalVersion.mockResolvedValue(Ok(mockBot));

      const result = await service.getGlobalSpecificVersion(botName, version);

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockBot);
      expect(mockRepository.getSpecificGlobalVersion).toHaveBeenCalledWith(
        botName,
        version,
      );
    });
  });

});
