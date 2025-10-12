import { CustomBotRepository } from "../custom-bot.repository";
import {
  CustomBot,
  CustomBotConfig,
  MarketplaceMetadata,
} from "../custom-bot.types";
import { Result } from "@/common";

// Mock Drizzle database and dependencies
jest.mock("@/database/connection", () => ({
  getDatabase: jest.fn(),
  getTables: jest.fn(),
}));

jest.mock("drizzle-orm", () => ({
  eq: jest.fn(),
  and: jest.fn(),
  desc: jest.fn(),
  asc: jest.fn(),
}));

jest.mock("@paralleldrive/cuid2", () => ({
  createId: jest.fn(() => "mock-id"),
}));

describe("CustomBotRepository", () => {
  let repository: CustomBotRepository;
  let mockDb: any;
  let mockTables: any;
  let mockTable: any;

  beforeEach(() => {
    // Mock table operations
    mockTable = {
      userId: { name: "userId" },
      id: { name: "id" },
      name: { name: "name" },
      version: { name: "version" },
      createdAt: { name: "createdAt" },
      updatedAt: { name: "updatedAt" },
    };

    mockTables = {
      customBots: mockTable,
    };

    // Mock database operations - simple approach
    mockDb = {
      select: jest.fn().mockReturnThis(),
      insert: jest.fn().mockReturnThis(),
      update: jest.fn().mockReturnThis(),
      delete: jest.fn().mockReturnThis(),
      from: jest.fn().mockReturnThis(),
      where: jest.fn().mockReturnThis(),
      orderBy: jest.fn().mockReturnThis(),
      limit: jest.fn().mockReturnThis(),
      values: jest.fn().mockReturnThis(),
      set: jest.fn().mockReturnThis(),
      returning: jest.fn().mockResolvedValue([{ id: "test-id" }]),
    };

    // Mock the database connection
    const { getDatabase, getTables } = require("@/database/connection");
    getDatabase.mockReturnValue(mockDb);
    getTables.mockReturnValue(mockTables);

    repository = new CustomBotRepository();
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  describe("userBotExists", () => {
    it("should return true when bot exists", async () => {
      // Mock the findByKey method directly
      const originalFindByKey = repository.findByKey;
      repository.findByKey = jest.fn().mockResolvedValue({
        success: true,
        data: [{ id: "test-id", name: "test-bot", userId: "user123" }],
      });

      const result = await repository.userBotExists("user123", "test-bot");

      expect(result.success).toBe(true);
      expect(result.data).toBe(true);

      // Restore original method
      repository.findByKey = originalFindByKey;
    });

    it("should return false when bot does not exist", async () => {
      // Mock the findByKey method to return empty array
      const originalFindByKey = repository.findByKey;
      repository.findByKey = jest.fn().mockResolvedValue({
        success: true,
        data: [],
      });

      const result = await repository.userBotExists(
        "user123",
        "non-existent-bot",
      );

      expect(result.success).toBe(true);
      expect(result.data).toBe(false);

      // Restore original method
      repository.findByKey = originalFindByKey;
    });
  });

  describe("globalBotExists", () => {
    it("should return true when global bot exists", async () => {
      // Mock the findGlobalByKey method directly
      const originalFindGlobalByKey = repository.findGlobalByKey;
      repository.findGlobalByKey = jest.fn().mockResolvedValue({
        success: true,
        data: [{ id: "test-id", name: "test-bot", userId: "user123" }],
      });

      const result = await repository.globalBotExists("test-bot");

      expect(result.success).toBe(true);
      expect(result.data).toBe(true);

      // Restore original method
      repository.findGlobalByKey = originalFindGlobalByKey;
    });

    it("should return false when global bot does not exist", async () => {
      // Mock the findGlobalByKey method to return empty array
      const originalFindGlobalByKey = repository.findGlobalByKey;
      repository.findGlobalByKey = jest.fn().mockResolvedValue({
        success: true,
        data: [],
      });

      const result = await repository.globalBotExists("non-existent-bot");

      expect(result.success).toBe(true);
      expect(result.data).toBe(false);

      // Restore original method
      repository.findGlobalByKey = originalFindGlobalByKey;
    });
  });

  describe("userVersionExists", () => {
    it("should return true when specific version exists", async () => {
      const mockBotData = {
        id: "test-id",
        name: "test-bot",
        version: "1.0.0",
        userId: "user123",
      };

      // Mock the specific findByKeyAndVersion method call
      const originalFindByKeyAndVersion = repository.findByKeyAndVersion;
      repository.findByKeyAndVersion = jest.fn().mockResolvedValue({
        success: true,
        data: mockBotData,
      });

      const result = await repository.userVersionExists(
        "user123",
        "test-bot",
        "1.0.0",
      );

      expect(result.success).toBe(true);
      expect(result.data).toBe(true);

      // Restore original method
      repository.findByKeyAndVersion = originalFindByKeyAndVersion;
    });

    it("should return false when version does not exist", async () => {
      // Mock the specific findByKeyAndVersion method call to return not found
      const originalFindByKeyAndVersion = repository.findByKeyAndVersion;
      repository.findByKeyAndVersion = jest.fn().mockResolvedValue({
        success: false,
        error: "Not found",
      });

      const result = await repository.userVersionExists(
        "user123",
        "test-bot",
        "2.0.0",
      );

      expect(result.success).toBe(true);
      expect(result.data).toBe(false);

      // Restore original method
      repository.findByKeyAndVersion = originalFindByKeyAndVersion;
    });
  });

  describe("globalVersionExists", () => {
    it("should return true when global version exists", async () => {
      const mockBotData = {
        id: "test-id",
        name: "test-bot",
        version: "1.0.0",
        userId: "user123",
      };

      // Mock the specific findGlobalByKeyAndVersion method call
      const originalFindGlobalByKeyAndVersion =
        repository.findGlobalByKeyAndVersion;
      repository.findGlobalByKeyAndVersion = jest.fn().mockResolvedValue({
        success: true,
        data: mockBotData,
      });

      const result = await repository.globalVersionExists("test-bot", "1.0.0");

      expect(result.success).toBe(true);
      expect(result.data).toBe(true);

      // Restore original method
      repository.findGlobalByKeyAndVersion = originalFindGlobalByKeyAndVersion;
    });

    it("should return false when global version does not exist", async () => {
      // Mock the specific findGlobalByKeyAndVersion method call to return not found
      const originalFindGlobalByKeyAndVersion =
        repository.findGlobalByKeyAndVersion;
      repository.findGlobalByKeyAndVersion = jest.fn().mockResolvedValue({
        success: false,
        error: "Not found",
      });

      const result = await repository.globalVersionExists("test-bot", "2.0.0");

      expect(result.success).toBe(true);
      expect(result.data).toBe(false);

      // Restore original method
      repository.findGlobalByKeyAndVersion = originalFindGlobalByKeyAndVersion;
    });
  });

  describe("getUserCustomBots", () => {
    it("should return empty array when user has no bots", async () => {
      // Mock the findAll method to return empty array
      const originalFindAll = repository.findAll;
      repository.findAll = jest.fn().mockResolvedValue({
        success: true,
        data: [],
      });

      const result = await repository.getUserCustomBots("user123");

      expect(result.success).toBe(true);
      expect(result.data).toEqual([]);

      // Restore original method
      repository.findAll = originalFindAll;
    });

    it("should return single bot with multiple versions grouped correctly", async () => {
      const mockBots: CustomBot[] = [
        {
          id: "v2-id",
          name: "arbitrage-bot",
          version: "2.0.0",
          config: {
            name: "arbitrage-bot",
            version: "2.0.0",
            description: "Advanced arbitrage bot",
            type: "realtime",
            runtime: "python3.11",
            author: "user@example.com",
            entrypoints: { bot: "main.py", backtest: "backtest.py" },
            schema: { bot: {}, backtest: {} },
            readme: "Advanced bot with multiple features",
          },
          status: "approved",
          filePath: "/local/path/arbitrage-bot/2.0.0/file.zip",
          userId: "user123",
          marketplace: null,
          createdAt: new Date("2023-02-01T10:00:00Z"),
          updatedAt: new Date("2023-02-01T10:00:00Z"),
        },
        {
          id: "v1-id",
          name: "arbitrage-bot",
          version: "1.0.0",
          config: {
            name: "arbitrage-bot",
            version: "1.0.0",
            description: "Basic arbitrage bot",
            type: "realtime",
            runtime: "python3.11",
            author: "user@example.com",
            entrypoints: { bot: "main.py", backtest: "backtest.py" },
            schema: { bot: {}, backtest: {} },
            readme: "Advanced bot with multiple features",
          },
          status: "approved",
          filePath: "/local/path/arbitrage-bot/1.0.0/file.zip",
          userId: "user123",
          marketplace: null,
          createdAt: new Date("2023-01-01T10:00:00Z"),
          updatedAt: new Date("2023-01-01T10:00:00Z"),
        },
      ] as CustomBot[];

      // Mock the findAll method to return mock data
      const originalFindAll = repository.findAll;
      repository.findAll = jest.fn().mockResolvedValue({
        success: true,
        data: mockBots,
      });

      const result = await repository.getUserCustomBots("user123");

      // Restore original method
      repository.findAll = originalFindAll;

      expect(result.success).toBe(true);
      expect(result.data).toHaveLength(1);

      const botWithVersions = result.data![0];
      expect(botWithVersions.name).toBe("arbitrage-bot");
      expect(botWithVersions.latestVersion).toBe("2.0.0");
      expect(botWithVersions.versions).toHaveLength(2);
      expect(botWithVersions.versions[0].version).toBe("2.0.0");
      expect(botWithVersions.versions[1].version).toBe("1.0.0");
      expect(botWithVersions.createdAt).toEqual(
        new Date("2023-01-01T10:00:00Z"),
      ); // First created
      expect(botWithVersions.updatedAt).toEqual(
        new Date("2023-02-01T10:00:00Z"),
      ); // Latest updated
    });

    it("should return multiple bots with correct grouping and sorting", async () => {
      const mockBots: CustomBot[] = [
        // Trading Bot (newer updates)
        {
          id: "trading-v2-id",
          name: "trading-bot",
          version: "2.1.0",
          config: {
            name: "trading-bot",
            version: "2.1.0",
            description: "Advanced trading bot",
            type: "scheduled",
            runtime: "python3.11",
            author: "user@example.com",
            entrypoints: { bot: "trade.py", backtest: "backtest.py" },
            schema: { bot: {}, backtest: {} },
            readme: "Advanced trading bot with multiple features",
          },
          status: "approved",
          filePath: "/local/path/trading-bot/2.1.0/file.zip",
          userId: "user123",
          marketplace: null,
          createdAt: new Date("2023-03-15T10:00:00Z"),
          updatedAt: new Date("2023-03-15T10:00:00Z"),
        },
        {
          id: "trading-v1-id",
          name: "trading-bot",
          version: "2.0.0",
          config: {
            name: "trading-bot",
            version: "2.0.0",
            description: "Basic trading bot",
            type: "scheduled",
            runtime: "python3.11",
            author: "user@example.com",
            entrypoints: { bot: "trade.py", backtest: "backtest.py" },
            schema: { bot: {}, backtest: {} },
            readme: "Basic trading bot with minimal features",
          },
          status: "approved",
          filePath: "/local/path/trading-bot/2.0.0/file.zip",
          userId: "user123",
          marketplace: null,
          createdAt: new Date("2023-01-15T10:00:00Z"),
          updatedAt: new Date("2023-01-15T10:00:00Z"),
        },
        // Arbitrage Bot (older updates)
        {
          id: "arb-v1-id",
          name: "arbitrage-bot",
          version: "1.0.0",
          config: {
            name: "arbitrage-bot",
            version: "1.0.0",
            description: "Arbitrage bot",
            type: "realtime",
            runtime: "python3.11",
            author: "user@example.com",
            entrypoints: { bot: "arbitrage.py", backtest: "backtest.py" },
            schema: { bot: {}, backtest: {} },
            readme: "Basic arbitrage bot with minimal features",
          },
          status: "approved",
          filePath: "/local/path/arbitrage-bot/1.0.0/file.zip",
          userId: "user123",
          createdAt: new Date("2023-02-01T10:00:00Z"),
          updatedAt: new Date("2023-02-01T10:00:00Z"),
        },
      ] as CustomBot[];

      // Mock the findAll method to return mock data
      const originalFindAll = repository.findAll;
      repository.findAll = jest.fn().mockResolvedValue({
        success: true,
        data: mockBots,
      });

      const result = await repository.getUserCustomBots("user123");

      // Restore original method
      repository.findAll = originalFindAll;

      expect(result.success).toBe(true);
      expect(result.data).toHaveLength(2);

      // Should be sorted by latest update time descending
      expect(result.data![0].name).toBe("trading-bot"); // More recent update
      expect(result.data![1].name).toBe("arbitrage-bot"); // Older update

      // Check trading bot details
      const tradingBot = result.data![0];
      expect(tradingBot.latestVersion).toBe("2.1.0");
      expect(tradingBot.versions).toHaveLength(2);
      expect(tradingBot.versions[0].version).toBe("2.1.0"); // Latest first
      expect(tradingBot.versions[1].version).toBe("2.0.0");

      // Check arbitrage bot details
      const arbitrageBot = result.data![1];
      expect(arbitrageBot.latestVersion).toBe("1.0.0");
      expect(arbitrageBot.versions).toHaveLength(1);
    });

    it("should handle repository errors gracefully", async () => {
      // Mock findAll to return an error
      const originalFindAll = repository.findAll;
      repository.findAll = jest.fn().mockResolvedValue({
        success: false,
        error: "Database connection failed",
      });

      const result = await repository.getUserCustomBots("user123");

      expect(result.success).toBe(false);
      expect(result.error).toBe("Database connection failed");

      // Restore original method
      repository.findAll = originalFindAll;
    });

    it("should handle unexpected errors gracefully", async () => {
      // Mock findAll to throw an exception
      const originalFindAll = repository.findAll;
      repository.findAll = jest
        .fn()
        .mockRejectedValue(new Error("Unexpected error"));

      const result = await repository.getUserCustomBots("user123");

      expect(result.success).toBe(false);
      expect(result.error).toBe("Unexpected error");

      // Restore original method
      repository.findAll = originalFindAll;
    });
  });

  describe("getAllUserVersions", () => {
    it("should return all versions with proper structure", async () => {
      const mockBots = [
        {
          id: "v2-id",
          name: "test-bot",
          version: "2.0.0",
          config: { name: "test-bot", version: "2.0.0" },
          filePath: "/local/path/v2.zip",
          userId: "user123",
          createdAt: new Date("2023-02-01"),
          updatedAt: new Date("2023-02-01"),
        },
        {
          id: "v1-id",
          name: "test-bot",
          version: "1.0.0",
          config: { name: "test-bot", version: "1.0.0" },
          filePath: "/local/path/v1.zip",
          userId: "user123",
          createdAt: new Date("2023-01-01"),
          updatedAt: new Date("2023-01-01"),
        },
      ];

      // Mock the findByKey method to return mock data
      const originalFindByKey = repository.findByKey;
      repository.findByKey = jest.fn().mockResolvedValue({
        success: true,
        data: mockBots,
      });

      const result = await repository.getAllUserVersions("user123", "test-bot");

      // Restore original method
      repository.findByKey = originalFindByKey;

      expect(result.success).toBe(true);
      expect(result.data).toBeDefined();
      expect(result.data!.name).toBe("test-bot");
      expect(result.data!.latestVersion).toBe("2.0.0");
      expect(result.data!.versions).toHaveLength(2);
      expect(result.data!.versions[0].version).toBe("2.0.0");
      expect(result.data!.versions[1].version).toBe("1.0.0");
    });

    it("should return error when bot not found", async () => {
      // Mock the findByKey method to return empty array
      const originalFindByKey = repository.findByKey;
      repository.findByKey = jest.fn().mockResolvedValue({
        success: true,
        data: [],
      });

      const result = await repository.getAllUserVersions(
        "user123",
        "non-existent",
      );

      // Restore original method
      repository.findByKey = originalFindByKey;

      expect(result.success).toBe(false);
      expect(result.error).toBe("Bot not found");
    });
  });

  describe("getAllGlobalVersions", () => {
    it("should return all global versions with proper structure", async () => {
      const mockBots = [
        {
          id: "global-v2-id",
          name: "global-bot",
          version: "2.0.0",
          config: { name: "global-bot", version: "2.0.0" },
          filePath: "/local/path/global-v2.zip",
          userId: "user456",
          createdAt: new Date("2023-02-01"),
          updatedAt: new Date("2023-02-01"),
        },
        {
          id: "global-v1-id",
          name: "global-bot",
          version: "1.0.0",
          config: { name: "global-bot", version: "1.0.0" },
          filePath: "/local/path/global-v1.zip",
          userId: "user456",
          createdAt: new Date("2023-01-01"),
          updatedAt: new Date("2023-01-01"),
        },
      ];

      // Mock the findGlobalByKey method to return mock data
      const originalFindGlobalByKey = repository.findGlobalByKey;
      repository.findGlobalByKey = jest.fn().mockResolvedValue({
        success: true,
        data: mockBots,
      });

      const result = await repository.getAllGlobalVersions("global-bot");

      // Restore original method
      repository.findGlobalByKey = originalFindGlobalByKey;

      expect(result.success).toBe(true);
      expect(result.data).toBeDefined();
      expect(result.data!.name).toBe("global-bot");
      expect(result.data!.latestVersion).toBe("2.0.0");
      expect(result.data!.versions).toHaveLength(2);
    });

    it("should return error when global bot not found", async () => {
      // Mock the findGlobalByKey method to return empty array
      const originalFindGlobalByKey = repository.findGlobalByKey;
      repository.findGlobalByKey = jest.fn().mockResolvedValue({
        success: true,
        data: [],
      });

      const result = await repository.getAllGlobalVersions("non-existent");

      // Restore original method
      repository.findGlobalByKey = originalFindGlobalByKey;

      expect(result.success).toBe(false);
      expect(result.error).toBe("Bot not found");
    });
  });

  describe("getSpecificUserVersion", () => {
    it("should return specific user version", async () => {
      const mockBotData = {
        id: "test-id",
        name: "test-bot",
        version: "1.0.0",
        userId: "user123",
        config: { name: "test-bot", version: "1.0.0" },
        filePath: "/local/path/test.zip",
        createdAt: new Date("2023-01-01"),
        updatedAt: new Date("2023-01-01"),
      };

      // Mock the specific findByKeyAndVersion method call
      const originalFindByKeyAndVersion = repository.findByKeyAndVersion;
      repository.findByKeyAndVersion = jest.fn().mockResolvedValue({
        success: true,
        data: mockBotData,
      });

      const result = await repository.getSpecificUserVersion(
        "user123",
        "test-bot",
        "1.0.0",
      );

      expect(result.success).toBe(true);
      expect(result.data).toBeDefined();
      expect(result.data!.version).toBe("1.0.0");

      // Restore original method
      repository.findByKeyAndVersion = originalFindByKeyAndVersion;
    });
  });

  describe("getSpecificGlobalVersion", () => {
    it("should return specific global version", async () => {
      const mockBotData = {
        id: "global-test-id",
        name: "global-bot",
        version: "1.0.0",
        userId: "user456",
        config: { name: "global-bot", version: "1.0.0" },
        filePath: "/local/path/global-test.zip",
        createdAt: new Date("2023-01-01"),
        updatedAt: new Date("2023-01-01"),
      };

      // Mock the specific findGlobalByKeyAndVersion method call
      const originalFindGlobalByKeyAndVersion =
        repository.findGlobalByKeyAndVersion;
      repository.findGlobalByKeyAndVersion = jest.fn().mockResolvedValue({
        success: true,
        data: mockBotData,
      });

      const result = await repository.getSpecificGlobalVersion(
        "global-bot",
        "1.0.0",
      );

      expect(result.success).toBe(true);
      expect(result.data).toBeDefined();
      expect(result.data!.version).toBe("1.0.0");

      // Restore original method
      repository.findGlobalByKeyAndVersion = originalFindGlobalByKeyAndVersion;
    });
  });

  describe("isVersionNewer", () => {
    it("should return true for newer major version", () => {
      const result = repository.isVersionNewer("1.0.0", "2.0.0");
      expect(result).toBe(true);
    });

    it("should return true for newer minor version", () => {
      const result = repository.isVersionNewer("1.0.0", "1.1.0");
      expect(result).toBe(true);
    });

    it("should return true for newer patch version", () => {
      const result = repository.isVersionNewer("1.0.0", "1.0.1");
      expect(result).toBe(true);
    });

    it("should return false for same version", () => {
      const result = repository.isVersionNewer("1.0.0", "1.0.0");
      expect(result).toBe(false);
    });

    it("should return false for older version", () => {
      const result = repository.isVersionNewer("2.0.0", "1.0.0");
      expect(result).toBe(false);
    });
  });

  describe("createNewGlobalVersion", () => {
    it("should create new version successfully", async () => {
      const config: CustomBotConfig = {
        name: "test-bot",
        version: "1.0.0",
        description: "Test bot",
        author: "Test Author",
        type: "scheduled",
        runtime: "python3.11",
        entrypoints: { bot: "main.py", backtest: "backtest.py" },
        schema: { bot: {}, backtest: {} },
        readme: "Test readme",
        metadata: { tags: ["test"], license: "MIT" },
      };
      const botData = {
        name: "test-bot",
        version: "1.0.0",
        config: config,
        userId: "user123",
        filePath: "/local/path/test.zip",
      };

      // Mock successful database operations
      mockDb.from.mockReturnValue(mockDb);
      mockDb.orderBy.mockResolvedValue([{ ...botData, id: "new-id" }]);

      const result = await repository.createNewGlobalVersion(
        "user123",
        botData,
      );

      expect(result.success).toBe(true);
      expect(result.data).toBeDefined();
      expect(mockDb.insert).toHaveBeenCalled();
    });
  });

  describe("checkUserOwnership", () => {
    it("should return true if user owns the bot", async () => {
      const mockBotData = {
        id: "test-id",
        name: "test-bot",
        userId: "user123",
      };

      // Mock the findGlobalByKey method directly
      const originalFindGlobalByKey = repository.findGlobalByKey;
      repository.findGlobalByKey = jest.fn().mockResolvedValue({
        success: true,
        data: [mockBotData],
      });

      const result = await repository.checkUserOwnership("user123", "test-bot");

      // Restore original method
      repository.findGlobalByKey = originalFindGlobalByKey;

      expect(result.success).toBe(true);
      expect(result.data).toBe(true);
    });

    it("should return error if user does not own the bot", async () => {
      const mockBotData = {
        id: "test-id",
        name: "test-bot",
        userId: "otherUser",
      };

      // Mock the findGlobalByKey method with different user
      const originalFindGlobalByKey = repository.findGlobalByKey;
      repository.findGlobalByKey = jest.fn().mockResolvedValue({
        success: true,
        data: [mockBotData],
      });

      const result = await repository.checkUserOwnership("user123", "test-bot");

      // Restore original method
      repository.findGlobalByKey = originalFindGlobalByKey;

      expect(result.success).toBe(false);
      expect(result.error).toBe("Insufficient permissions");
    });

    it("should return error if bot is not found", async () => {
      // Mock the findGlobalByKey method to return empty array
      const originalFindGlobalByKey = repository.findGlobalByKey;
      repository.findGlobalByKey = jest.fn().mockResolvedValue({
        success: true,
        data: [],
      });

      const result = await repository.checkUserOwnership(
        "user123",
        "non-existent-bot",
      );

      // Restore original method
      repository.findGlobalByKey = originalFindGlobalByKey;

      expect(result.success).toBe(false);
      expect(result.error).toBe("Bot not found");
    });
  });
});
