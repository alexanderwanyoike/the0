import { Test, TestingModule } from "@nestjs/testing";
import { INestApplication } from "@nestjs/common";
import request from "supertest";
import { BacktestModule } from "../backtest.module";
import { BacktestRepository } from "../backtest.repository";
import { BacktestValidator } from "../backtest.validator";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";
import { NatsService } from "@/nats/nats.service";
import { Ok, Failure } from "@/common/result";
import { Backtest } from "../entities/backtest.entity";
import { CustomBot } from "@/custom-bot/custom-bot.types";

describe("Backtest API Integration Tests", () => {
  let app: INestApplication;
  let repository: jest.Mocked<BacktestRepository>;
  let backtestValidator: jest.Mocked<BacktestValidator>;
  let customBotService: jest.Mocked<CustomBotService>;

  const validBacktestConfig = {
    name: "Test Backtest",
    config: {
      type: "scheduled/test-bot",
      version: "1.0.0",
      schedule: "0 9 * * 1-5", // Valid cron for scheduled bots
      symbol: "AAPL",
      strategy: "sma-crossover",
    },
  };

  const mockPublishedBot: CustomBot = {
    id: "published-bot-id",
    name: "test-bot",
    version: "1.0.0",
    userId: "other-user-id",
    status: "published",
    marketplace: {
      isPublished: true,
      price: 0,
      description: "Test marketplace bot",
      tags: ["test"],
      installCount: 100,
      totalReviews: 5,
      revenue: 0,
    },
    config: {
      name: "test-bot",
      description: "Test bot for integration testing",
      version: "1.0.0",
      type: "scheduled",
      runtime: "python3.11",
      author: "Test Author",
      entrypoints: {
        bot: "main.py",
        backtest: "backtest.py",
      },
      schema: {
        backtest: {
          type: "object",
          properties: {
            type: { type: "string" },
            version: { type: "string" },
            schedule: { type: "string" },
            symbol: { type: "string" },
            strategy: { type: "string" },
          },
          required: ["type", "version", "schedule", "symbol", "strategy"],
        },
        bot: { type: "object" },
      },
      readme: "Test bot readme",
    },
    filePath: "gs://test-bucket/test-bot.zip",
    createdAt: new Date(),
    updatedAt: new Date(),
  };

  const mockApprovedBotOwnedByUser: CustomBot = {
    ...mockPublishedBot,
    id: "approved-bot-id",
    userId: "test-user-123", // Owned by test user
    status: "approved",
    marketplace: {
      isPublished: false,
      price: 0,
      description: "Test approved bot",
      tags: ["test"],
      installCount: 0,
      totalReviews: 0,
      revenue: 0,
    },
  };

  // Mock auth middleware
  const mockAuthMiddleware = (req: any, res: any, next: any) => {
    req.userId = "test-user-123";
    req.user = { uid: "test-user-123" };
    next();
  };

  beforeAll(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [BacktestModule],
    })
      .overrideProvider(BacktestRepository)
      .useValue({
        create: jest.fn(),
        findAll: jest.fn(),
        findOne: jest.fn(),
        remove: jest.fn(),
        update: jest.fn(),
      })
      .overrideProvider(BacktestValidator)
      .useValue({
        validate: jest.fn(),
      })
      .overrideProvider(CustomBotService)
      .useValue({
        getGlobalSpecificVersion: jest.fn(),
      })
      .overrideGuard(AuthCombinedGuard)
      .useValue({
        canActivate: jest.fn(() => true),
      })
      .overrideProvider(NatsService)
      .useValue({
        publish: jest.fn().mockResolvedValue(Ok(null)),
      })
      .compile();

    app = moduleFixture.createNestApplication();

    // Apply mock auth middleware
    app.use("/backtest", mockAuthMiddleware);

    await app.init();

    repository = moduleFixture.get(BacktestRepository);
    backtestValidator = moduleFixture.get(BacktestValidator);
    customBotService = moduleFixture.get(CustomBotService);
  });

  afterAll(async () => {
    await app.close();
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  describe("POST /backtest (Create Backtest)", () => {
    it("should create backtest for published custom bot successfully", async () => {
      const mockCreatedBacktest: Backtest = {
        id: "new-backtest-id",
        name: "Test Backtest",
        config: validBacktestConfig.config,
        analysis: "",
        status: "pending",
        userId: "test-user-123",
        customBotId: mockPublishedBot.id,
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      // Mock custom bot service to return published bot
      customBotService.getGlobalSpecificVersion.mockResolvedValue(
        Ok(mockPublishedBot),
      );

      // Mock validator to return success
      backtestValidator.validate.mockResolvedValue(Ok(true));

      // Mock repository to return success
      repository.create.mockResolvedValue(Ok(mockCreatedBacktest));

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(validBacktestConfig)
        .expect(201);

      expect(response.body.id).toBe("new-backtest-id");
      expect(response.body.customBotId).toBe(mockPublishedBot.id);
      expect(response.body.status).toBe("pending");

      // Verify service calls
      expect(customBotService.getGlobalSpecificVersion).toHaveBeenCalledWith(
        "test-bot",
        "1.0.0",
      );
      expect(backtestValidator.validate).toHaveBeenCalledWith(
        validBacktestConfig.config,
        mockPublishedBot,
      );
      expect(repository.create).toHaveBeenCalledWith({
        ...validBacktestConfig,
        status: "pending",
        userId: "test-user-123",
        customBotId: mockPublishedBot.id,
      });
    });

    it("should create backtest for user-owned approved custom bot successfully", async () => {
      const mockCreatedBacktest: Backtest = {
        id: "approved-backtest-id",
        name: "Test Backtest",
        config: validBacktestConfig.config,
        analysis: "",
        status: "pending",
        userId: "test-user-123",
        customBotId: mockApprovedBotOwnedByUser.id,
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      // Mock custom bot service to return user's approved bot
      customBotService.getGlobalSpecificVersion.mockResolvedValue(
        Ok(mockApprovedBotOwnedByUser),
      );

      // Mock validator to return success
      backtestValidator.validate.mockResolvedValue(Ok(true));

      // Mock repository to return success
      repository.create.mockResolvedValue(Ok(mockCreatedBacktest));

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(validBacktestConfig)
        .expect(201);

      expect(response.body.id).toBe("approved-backtest-id");
      expect(response.body.customBotId).toBe(mockApprovedBotOwnedByUser.id);
      expect(response.body.status).toBe("pending");
    });

    it("should return 400 when bot type is missing", async () => {
      const invalidConfig = {
        name: "Test Backtest",
        config: {
          version: "1.0.0",
          symbol: "AAPL",
        },
      };

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(invalidConfig)
        .expect(400);

      expect(response.body.message).toBe("Bot type is required");
    });

    it("should return 400 when bot version is missing", async () => {
      const invalidConfig = {
        name: "Test Backtest",
        config: {
          type: "scheduled/test-bot",
          symbol: "AAPL",
        },
      };

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(invalidConfig)
        .expect(400);

      expect(response.body.message).toBe("Bot version is required");
    });

    it("should return 400 when bot type format is invalid", async () => {
      const invalidConfig = {
        name: "Test Backtest",
        config: {
          type: "invalid-format",
          version: "1.0.0",
        },
      };

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(invalidConfig)
        .expect(400);

      expect(response.body.message).toBe(
        "Invalid bot type format. Expected format: type/name",
      );
    });

    it("should return 400 when custom bot is not found", async () => {
      customBotService.getGlobalSpecificVersion.mockResolvedValue(
        Failure("Custom bot not found"),
      );

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(validBacktestConfig)
        .expect(400);

      expect(response.body.message).toBe(
        "Custom bot not found: test-bot@1.0.0",
      );
    });

    it("should return 400 when user lacks authorization for non-published bot", async () => {
      const unauthorizedBot: CustomBot = {
        ...mockPublishedBot,
        userId: "other-user-id",
        status: "approved", // Not published, not owned by user
        marketplace: {
          isPublished: false,
          price: 0,
          description: "Test approved bot",
          tags: ["test"],
          installCount: 0,
          totalReviews: 0,
          revenue: 0,
        },
      };

      customBotService.getGlobalSpecificVersion.mockResolvedValue(
        Ok(unauthorizedBot),
      );

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(validBacktestConfig)
        .expect(400);

      expect(response.body.message).toBe(
        "Insufficient permissions: Can only backtest published bots or your own approved bots",
      );
    });

    it("should return 400 when config validation fails", async () => {
      customBotService.getGlobalSpecificVersion.mockResolvedValue(
        Ok(mockPublishedBot),
      );

      // Mock validator to return validation errors
      backtestValidator.validate.mockResolvedValue(
        Failure(["schedule is required", "symbol must be a string"]),
      );

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(validBacktestConfig)
        .expect(400);

      expect(response.body.message).toBe(
        "Validation failed: schedule is required, symbol must be a string",
      );
    });

    it("should return 400 when repository creation fails", async () => {
      customBotService.getGlobalSpecificVersion.mockResolvedValue(
        Ok(mockPublishedBot),
      );
      backtestValidator.validate.mockResolvedValue(Ok(true));
      repository.create.mockResolvedValue(
        Failure("Database connection failed"),
      );

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(validBacktestConfig)
        .expect(400);

      expect(response.body.message).toBe("Database connection failed");
    });
  });

  describe("GET /backtest (List Backtests)", () => {
    it("should return all user backtests successfully", async () => {
      const mockBacktests: Backtest[] = [
        {
          id: "backtest-1",
          name: "Backtest 1",
          config: validBacktestConfig.config,
          analysis: "Sample analysis",
          status: "completed",
          userId: "test-user-123",
          customBotId: mockPublishedBot.id,
          createdAt: new Date(),
          updatedAt: new Date(),
        },
        {
          id: "backtest-2",
          name: "Backtest 2",
          config: validBacktestConfig.config,
          analysis: "",
          status: "pending",
          userId: "test-user-123",
          customBotId: mockApprovedBotOwnedByUser.id,
          createdAt: new Date(),
          updatedAt: new Date(),
        },
      ];

      repository.findAll.mockResolvedValue(Ok(mockBacktests));

      const response = await request(app.getHttpServer())
        .get("/backtest")
        .expect(200);

      expect(response.body).toHaveLength(2);
      expect(response.body[0].id).toBe("backtest-1");
      expect(response.body[1].id).toBe("backtest-2");
      expect(repository.findAll).toHaveBeenCalledWith("test-user-123");
    });

    it("should return 404 when no backtests found", async () => {
      repository.findAll.mockResolvedValue(Failure("No backtests found"));

      const response = await request(app.getHttpServer())
        .get("/backtest")
        .expect(404);

      expect(response.body.message).toBe("No backtests found");
    });
  });

  describe("GET /backtest/:id (Get Specific Backtest)", () => {
    it("should return specific backtest successfully", async () => {
      const mockBacktest: Backtest = {
        id: "specific-backtest-id",
        name: "Specific Backtest",
        config: validBacktestConfig.config,
        analysis: "Detailed analysis results",
        status: "completed",
        userId: "test-user-123",
        customBotId: mockPublishedBot.id,
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      repository.findOne.mockResolvedValue(Ok(mockBacktest));

      const response = await request(app.getHttpServer())
        .get("/backtest/specific-backtest-id")
        .expect(200);

      expect(response.body.id).toBe("specific-backtest-id");
      expect(response.body.name).toBe("Specific Backtest");
      expect(response.body.status).toBe("completed");
      expect(repository.findOne).toHaveBeenCalledWith(
        "test-user-123",
        "specific-backtest-id",
      );
    });

    it("should return 404 when backtest not found", async () => {
      repository.findOne.mockResolvedValue(Failure("Backtest not found"));

      const response = await request(app.getHttpServer())
        .get("/backtest/non-existent-id")
        .expect(404);

      expect(response.body.message).toBe("Backtest not found");
    });
  });

  describe("DELETE /backtest/:id (Delete Backtest)", () => {
    it("should delete backtest successfully", async () => {
      repository.remove.mockResolvedValue(Ok(undefined));

      const response = await request(app.getHttpServer())
        .delete("/backtest/backtest-to-delete")
        .expect(200);

      expect(response.body).toEqual({});
      expect(repository.remove).toHaveBeenCalledWith(
        "test-user-123",
        "backtest-to-delete",
      );
    });

    it("should return 404 when backtest to delete not found", async () => {
      repository.remove.mockResolvedValue(
        Failure("Backtest not found for deletion"),
      );

      const response = await request(app.getHttpServer())
        .delete("/backtest/non-existent-id")
        .expect(404);

      expect(response.body.message).toBe("Backtest not found for deletion");
    });
  });

  describe("Authorization Scenarios", () => {
    it("should allow backtest creation for published bots by any user", async () => {
      const publishedBot: CustomBot = {
        ...mockPublishedBot,
        userId: "different-owner-id", // Different owner
        status: "published",
        marketplace: {
          isPublished: true,
          price: 0,
          description: "Test marketplace bot",
          tags: ["test"],
          installCount: 100,
          totalReviews: 5,
          revenue: 0,
        },
      };

      customBotService.getGlobalSpecificVersion.mockResolvedValue(
        Ok(publishedBot),
      );
      backtestValidator.validate.mockResolvedValue(Ok(true));
      repository.create.mockResolvedValue(
        Ok({
          id: "authorized-backtest-id",
          name: "Authorized Backtest",
          config: validBacktestConfig.config,
          analysis: "",
          status: "pending",
          userId: "test-user-123",
          customBotId: publishedBot.id,
          createdAt: new Date(),
          updatedAt: new Date(),
        }),
      );

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(validBacktestConfig)
        .expect(201);

      expect(response.body.customBotId).toBe(publishedBot.id);
    });

    it("should deny backtest creation for non-approved owned bots", async () => {
      const pendingBot: CustomBot = {
        ...mockApprovedBotOwnedByUser,
        status: "pending_review", // Not approved yet
      };

      customBotService.getGlobalSpecificVersion.mockResolvedValue(
        Ok(pendingBot),
      );

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(validBacktestConfig)
        .expect(400);

      expect(response.body.message).toBe(
        "Insufficient permissions: Can only backtest published bots or your own approved bots",
      );
    });

    it("should deny backtest creation for approved bots owned by others", async () => {
      const otherUserBot: CustomBot = {
        ...mockApprovedBotOwnedByUser,
        userId: "other-user-id", // Different owner
        status: "approved",
        marketplace: {
          isPublished: false,
          price: 0,
          description: "Test approved bot",
          tags: ["test"],
          installCount: 0,
          totalReviews: 0,
          revenue: 0,
        }, // Not published
      };

      customBotService.getGlobalSpecificVersion.mockResolvedValue(
        Ok(otherUserBot),
      );

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(validBacktestConfig)
        .expect(400);

      expect(response.body.message).toBe(
        "Insufficient permissions: Can only backtest published bots or your own approved bots",
      );
    });
  });

  describe("Validation Scenarios", () => {
    it("should validate scheduled bot requires cron schedule", async () => {
      const configWithoutSchedule = {
        name: "Test Backtest",
        config: {
          type: "scheduled/test-bot",
          version: "1.0.0",
          symbol: "AAPL",
          // Missing schedule
        },
      };

      customBotService.getGlobalSpecificVersion.mockResolvedValue(
        Ok(mockPublishedBot),
      );
      backtestValidator.validate.mockResolvedValue(
        Failure(["No schedule provided"]),
      );

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(configWithoutSchedule)
        .expect(400);

      expect(response.body.message).toBe(
        "Validation failed: No schedule provided",
      );
    });

    it("should validate against custom bot backtest schema", async () => {
      const configWithInvalidFields = {
        name: "Test Backtest",
        config: {
          type: "scheduled/test-bot",
          version: "1.0.0",
          schedule: "0 9 * * 1-5",
          invalidField: "should not be allowed",
        },
      };

      customBotService.getGlobalSpecificVersion.mockResolvedValue(
        Ok(mockPublishedBot),
      );
      backtestValidator.validate.mockResolvedValue(
        Failure(["invalidField is not allowed"]),
      );

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(configWithInvalidFields)
        .expect(400);

      expect(response.body.message).toBe(
        "Validation failed: invalidField is not allowed",
      );
    });
  });

  describe("End-to-end Workflow", () => {
    it("should complete full backtest lifecycle: create -> list -> get -> delete", async () => {
      // Step 1: Create backtest
      const mockCreatedBacktest: Backtest = {
        id: "e2e-backtest-id",
        name: "E2E Test Backtest",
        config: validBacktestConfig.config,
        analysis: "",
        status: "pending",
        userId: "test-user-123",
        customBotId: mockPublishedBot.id,
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      customBotService.getGlobalSpecificVersion.mockResolvedValueOnce(
        Ok(mockPublishedBot),
      );
      backtestValidator.validate.mockResolvedValueOnce(Ok(true));
      repository.create.mockResolvedValueOnce(Ok(mockCreatedBacktest));

      const createResponse = await request(app.getHttpServer())
        .post("/backtest")
        .send({
          name: "E2E Test Backtest",
          config: validBacktestConfig.config,
        })
        .expect(201);

      expect(createResponse.body.id).toBe("e2e-backtest-id");

      // Step 2: List backtests
      repository.findAll.mockResolvedValueOnce(Ok([mockCreatedBacktest]));

      const listResponse = await request(app.getHttpServer())
        .get("/backtest")
        .expect(200);

      expect(listResponse.body).toHaveLength(1);
      expect(listResponse.body[0].id).toBe("e2e-backtest-id");

      // Step 3: Get specific backtest
      repository.findOne.mockResolvedValueOnce(Ok(mockCreatedBacktest));

      const getResponse = await request(app.getHttpServer())
        .get("/backtest/e2e-backtest-id")
        .expect(200);

      expect(getResponse.body.id).toBe("e2e-backtest-id");
      expect(getResponse.body.name).toBe("E2E Test Backtest");

      // Step 4: Delete backtest
      repository.remove.mockResolvedValueOnce(Ok(undefined));

      await request(app.getHttpServer())
        .delete("/backtest/e2e-backtest-id")
        .expect(200);

      expect(repository.remove).toHaveBeenCalledWith(
        "test-user-123",
        "e2e-backtest-id",
      );
    });
  });

  describe("Error Handling", () => {
    it("should handle custom bot service errors gracefully", async () => {
      customBotService.getGlobalSpecificVersion.mockResolvedValue(
        Failure("Custom bot service unavailable"),
      );

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(validBacktestConfig)
        .expect(400);

      expect(response.body.message).toBe(
        "Custom bot not found: test-bot@1.0.0",
      );
    });

    it("should handle validator service errors gracefully", async () => {
      customBotService.getGlobalSpecificVersion.mockResolvedValue(
        Ok(mockPublishedBot),
      );
      backtestValidator.validate.mockResolvedValue(
        Failure(["Validation service error"]),
      );

      const response = await request(app.getHttpServer())
        .post("/backtest")
        .send(validBacktestConfig)
        .expect(400);

      expect(response.body.message).toBe(
        "Validation failed: Validation service error",
      );
    });

    it("should handle repository errors gracefully", async () => {
      repository.findAll.mockResolvedValue(Failure("Database connection lost"));

      const response = await request(app.getHttpServer())
        .get("/backtest")
        .expect(404);

      expect(response.body.message).toBe("Database connection lost");
    });
  });
});
