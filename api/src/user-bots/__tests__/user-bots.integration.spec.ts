import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import request from 'supertest';
import { UserBotsModule } from '../user-bots.module';
import { UserBotsService } from '../user-bots.service';
import { UserBotsRepository } from '../user-bots.repository';
import { CustomBotService } from '@/custom-bot/custom-bot.service';
import { AuthCombinedGuard } from '@/auth/auth-combined.guard';
import { Ok, Failure } from '@/common/result';
// Removed Stripe - not needed in OSS version

describe('UserBotsModule (Integration)', () => {
  let app: INestApplication;
  let userBotsService: UserBotsService;
  let userBotsRepository: UserBotsRepository;
  let customBotService: CustomBotService;

  const mockUser = { uid: 'test_user_123' };
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
  };

  beforeEach(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [UserBotsModule],
    })
      .overrideGuard(AuthCombinedGuard)
      .useValue({
        canActivate: jest.fn((context) => {
          const request = context.switchToHttp().getRequest();
          request.user = mockUser;
          return true;
        }),
      })
      .overrideProvider(UserBotsRepository)
      .useValue({
        findByUserId: jest.fn(),
        hasUserBot: jest.fn(),
        addBotToUser: jest.fn(),
      })
      .overrideProvider(CustomBotService)
      .useValue({
        getAllGlobalVersions: jest.fn(),
        // Removed incrementBotInstalls - marketplace functionality not in OSS version
      })
      .compile();

    app = moduleFixture.createNestApplication();
    await app.init();

    userBotsService = await moduleFixture.resolve<UserBotsService>(
      UserBotsService,
    );
    userBotsRepository =
      moduleFixture.get<UserBotsRepository>(UserBotsRepository);
    customBotService = moduleFixture.get<CustomBotService>(CustomBotService);
  });

  afterEach(async () => {
    await app.close();
  });

  describe('POST /user-bots/install/:botName', () => {
    it('should install a free bot successfully', async () => {
      jest
        .spyOn(customBotService, 'getAllGlobalVersions')
        .mockResolvedValue(Ok(mockCustomBotVersions as any));
      jest.spyOn(userBotsRepository, 'hasUserBot').mockResolvedValue(Ok(false));
      jest
        .spyOn(userBotsRepository, 'addBotToUser')
        .mockResolvedValue(Ok(mockUserBot as any));
      // Removed incrementBotInstalls spy - marketplace functionality not in OSS version

      const response = await request(app.getHttpServer())
        .post('/user-bots/install/test-bot')
        .expect(200);

      expect(response.body).toEqual({
        success: true,
        message: 'Free bot added to your collection',
      });
      expect(customBotService.getAllGlobalVersions).toHaveBeenCalledWith(
        'test-bot',
      );
      expect(userBotsRepository.hasUserBot).toHaveBeenCalledWith(
        mockUser.uid,
        'test-bot',
      );
      expect(userBotsRepository.addBotToUser).toHaveBeenCalledWith(
        mockUser.uid,
        'test-bot',
      );
      // Removed incrementBotInstalls expectation - marketplace functionality not in OSS version
    });

    it('should return 400 for missing bot name', async () => {
      const response = await request(app.getHttpServer())
        .post('/user-bots/install/')
        .expect(404);
    });

    it('should return 400 when bot is not free', async () => {
      const paidBot = {
        ...mockCustomBotVersions,
        versions: [
          {
            ...mockCustomBotVersions.versions[0],
            marketplace: { price: 1000, isPublished: true },
          },
        ],
      };

      jest
        .spyOn(customBotService, 'getAllGlobalVersions')
        .mockResolvedValue(Ok(paidBot as any));

      const response = await request(app.getHttpServer())
        .post('/user-bots/install/paid-bot')
        .expect(400);

      expect(response.body.message).toBe('Bot is not free');
    });

    it('should return 400 when user already owns the bot', async () => {
      jest
        .spyOn(customBotService, 'getAllGlobalVersions')
        .mockResolvedValue(Ok(mockCustomBotVersions as any));
      jest.spyOn(userBotsRepository, 'hasUserBot').mockResolvedValue(Ok(true));

      const response = await request(app.getHttpServer())
        .post('/user-bots/install/test-bot')
        .expect(400);

      expect(response.body.message).toBe('You already own this bot');
    });

    it('should return 400 when bot is not found', async () => {
      jest
        .spyOn(customBotService, 'getAllGlobalVersions')
        .mockResolvedValue(Failure('Bot not found'));

      const response = await request(app.getHttpServer())
        .post('/user-bots/install/non-existent-bot')
        .expect(400);

      expect(response.body.message).toBe('Failed to fetch bot information');
    });
  });

  describe('GET /user-bots', () => {
    it('should return user bots successfully', async () => {
      jest
        .spyOn(userBotsRepository, 'findByUserId')
        .mockResolvedValue(Ok([mockUserBot as any]));

      jest
        .spyOn(customBotService, 'getAllGlobalVersions')
        .mockResolvedValue(Ok(mockCustomBotVersions as any));

      const response = await request(app.getHttpServer())
        .get('/user-bots')
        .expect(200);

      expect(response.body).toEqual({
        success: true,
        data: [
          {
            ...mockUserBot,
            acquiredAt: mockUserBot.acquiredAt.toISOString(),
            customBot: mockCustomBotVersions,
          },
        ],
      });
      expect(userBotsRepository.findByUserId).toHaveBeenCalledWith(
        mockUser.uid,
        {
          limit: undefined,
          offset: undefined,
        },
      );
    });

    it('should return user bots with query parameters', async () => {
      jest
        .spyOn(userBotsRepository, 'findByUserId')
        .mockResolvedValue(Ok([mockUserBot as any]));

      jest
        .spyOn(customBotService, 'getAllGlobalVersions')
        .mockResolvedValue(Ok(mockCustomBotVersions as any));

      await request(app.getHttpServer())
        .get('/user-bots?limit=10&offset=5')
        .expect(200);

      expect(userBotsRepository.findByUserId).toHaveBeenCalledWith(
        mockUser.uid,
        {
          limit: 10,
          offset: 5,
        },
      );
    });

    it('should return empty array when user has no bots', async () => {
      jest.spyOn(userBotsRepository, 'findByUserId').mockResolvedValue(Ok([]));

      const response = await request(app.getHttpServer())
        .get('/user-bots')
        .expect(200);

      expect(response.body).toEqual({
        success: true,
        data: [],
      });
    });

    it('should return 400 when repository fails', async () => {
      jest
        .spyOn(userBotsRepository, 'findByUserId')
        .mockResolvedValue(Failure('Database error'));

      const response = await request(app.getHttpServer())
        .get('/user-bots')
        .expect(400);

      expect(response.body.message).toBe('Database error');
    });
  });

  describe('GET /user-bots/has/:botName', () => {
    it('should return true when user has the bot', async () => {
      jest.spyOn(userBotsRepository, 'hasUserBot').mockResolvedValue(Ok(true));

      const response = await request(app.getHttpServer())
        .get('/user-bots/has/test-bot')
        .expect(200);

      expect(response.body).toEqual({
        success: true,
        hasBot: true,
      });
      expect(userBotsRepository.hasUserBot).toHaveBeenCalledWith(
        mockUser.uid,
        'test-bot',
      );
    });

    it('should return false when user does not have the bot', async () => {
      jest.spyOn(userBotsRepository, 'hasUserBot').mockResolvedValue(Ok(false));

      const response = await request(app.getHttpServer())
        .get('/user-bots/has/test-bot')
        .expect(200);

      expect(response.body).toEqual({
        success: true,
        hasBot: false,
      });
    });

    it('should return 400 when repository fails', async () => {
      jest
        .spyOn(userBotsRepository, 'hasUserBot')
        .mockResolvedValue(Failure('Database error'));

      const response = await request(app.getHttpServer())
        .get('/user-bots/has/test-bot')
        .expect(400);

      expect(response.body.message).toBe('Database error');
    });

    it('should handle special characters in bot name', async () => {
      jest.spyOn(userBotsRepository, 'hasUserBot').mockResolvedValue(Ok(false));

      await request(app.getHttpServer())
        .get('/user-bots/has/bot-name_with-special.chars123')
        .expect(200);

      expect(userBotsRepository.hasUserBot).toHaveBeenCalledWith(
        mockUser.uid,
        'bot-name_with-special.chars123',
      );
    });
  });

  describe('Authentication', () => {
    it('should require authentication for install endpoint', async () => {
      const moduleFixture: TestingModule = await Test.createTestingModule({
        imports: [UserBotsModule],
      })
        .overrideGuard(AuthCombinedGuard)
        .useValue({
          canActivate: jest.fn(() => false),
        })
        .compile();

      const testApp = moduleFixture.createNestApplication();
      await testApp.init();

      await request(testApp.getHttpServer())
        .post('/user-bots/install/test-bot')
        .expect(403);

      await testApp.close();
    });

    it('should require authentication for getUserBots endpoint', async () => {
      const moduleFixture: TestingModule = await Test.createTestingModule({
        imports: [UserBotsModule],
      })
        .overrideGuard(AuthCombinedGuard)
        .useValue({
          canActivate: jest.fn(() => false),
        })
        .compile();

      const testApp = moduleFixture.createNestApplication();
      await testApp.init();

      await request(testApp.getHttpServer()).get('/user-bots').expect(403);

      await testApp.close();
    });

    it('should require authentication for hasBot endpoint', async () => {
      const moduleFixture: TestingModule = await Test.createTestingModule({
        imports: [UserBotsModule],
      })
        .overrideGuard(AuthCombinedGuard)
        .useValue({
          canActivate: jest.fn(() => false),
        })
        .compile();

      const testApp = moduleFixture.createNestApplication();
      await testApp.init();

      await request(testApp.getHttpServer())
        .get('/user-bots/has/test-bot')
        .expect(403);

      await testApp.close();
    });
  });

  describe('End-to-End Free Bot Installation Flow', () => {
    it('should handle complete free bot installation workflow', async () => {
      jest
        .spyOn(customBotService, 'getAllGlobalVersions')
        .mockResolvedValue(Ok(mockCustomBotVersions as any));
      jest.spyOn(userBotsRepository, 'hasUserBot').mockResolvedValue(Ok(false));
      jest
        .spyOn(userBotsRepository, 'addBotToUser')
        .mockResolvedValue(Ok(mockUserBot as any));
      // Removed incrementBotInstalls spy - marketplace functionality not in OSS version

      // Step 1: Check if user has bot (should be false)
      const hasBot1 = await request(app.getHttpServer())
        .get('/user-bots/has/test-bot')
        .expect(200);

      expect(hasBot1.body.hasBot).toBe(false);

      // Step 2: Install the bot
      const installResponse = await request(app.getHttpServer())
        .post('/user-bots/install/test-bot')
        .expect(200);

      expect(installResponse.body.success).toBe(true);

      // Step 3: Check if user has bot (should be true after installation)
      jest.spyOn(userBotsRepository, 'hasUserBot').mockResolvedValue(Ok(true));

      const hasBot2 = await request(app.getHttpServer())
        .get('/user-bots/has/test-bot')
        .expect(200);

      expect(hasBot2.body.hasBot).toBe(true);

      // Step 4: Get user bots (should include the installed bot)
      jest
        .spyOn(userBotsRepository, 'findByUserId')
        .mockResolvedValue(Ok([mockUserBot as any]));

      const userBots = await request(app.getHttpServer())
        .get('/user-bots')
        .expect(200);

      expect(userBots.body.data).toHaveLength(1);
      expect(userBots.body.data[0].customBotName).toBe('test-bot');
    });

    it('should prevent duplicate installations', async () => {
      jest
        .spyOn(customBotService, 'getAllGlobalVersions')
        .mockResolvedValue(Ok(mockCustomBotVersions as any));
      jest.spyOn(userBotsRepository, 'hasUserBot').mockResolvedValue(Ok(false));
      jest
        .spyOn(userBotsRepository, 'addBotToUser')
        .mockResolvedValue(Ok(mockUserBot as any));
      // Removed incrementBotInstalls spy - marketplace functionality not in OSS version

      // First installation should succeed
      await request(app.getHttpServer())
        .post('/user-bots/install/test-bot')
        .expect(200);

      // Second installation should fail (user already owns bot)
      jest.spyOn(userBotsRepository, 'hasUserBot').mockResolvedValue(Ok(true));

      const secondInstall = await request(app.getHttpServer())
        .post('/user-bots/install/test-bot')
        .expect(400);

      expect(secondInstall.body.message).toBe('You already own this bot');
    });
  });

  describe('Error Handling', () => {
    it('should handle service layer errors gracefully', async () => {
      jest
        .spyOn(customBotService, 'getAllGlobalVersions')
        .mockRejectedValue(new Error('Service unavailable'));

      const response = await request(app.getHttpServer())
        .post('/user-bots/install/test-bot')
        .expect(400);

      expect(response.body.message).toContain('Failed to purchase free bot');
    });

    it('should handle repository errors gracefully', async () => {
      jest
        .spyOn(userBotsRepository, 'findByUserId')
        .mockRejectedValue(new Error('Repository error'));

      const response = await request(app.getHttpServer())
        .get('/user-bots')
        .expect(400);

      expect(response.body.message).toContain('Failed to fetch user bots');
    });
  });

  describe('Performance and Pagination', () => {
    it('should handle large datasets with pagination', async () => {
      const largeBotList = Array.from({ length: 100 }, (_, i) => ({
        id: `userbot_${i}`,
        customBotName: `bot-${i}`,
        acquiredAt: new Date(`2023-01-${(i % 31) + 1}`),
      }));

      jest
        .spyOn(customBotService, 'getAllGlobalVersions')
        .mockResolvedValue(Ok(mockCustomBotVersions as any));

      jest
        .spyOn(userBotsRepository, 'findByUserId')
        .mockResolvedValue(Ok(largeBotList.slice(0, 25) as any));

      const response = await request(app.getHttpServer())
        .get('/user-bots?limit=25&offset=0')
        .expect(200);

      expect(response.body.data).toHaveLength(25);
      expect(userBotsRepository.findByUserId).toHaveBeenCalledWith(
        mockUser.uid,
        { limit: 25, offset: 0 },
      );
    });
  });
});
