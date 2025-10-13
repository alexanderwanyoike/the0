// Mock MinIO for OSS version
jest.mock("minio", () => ({
  Client: jest.fn().mockImplementation(() => ({
    getObject: jest.fn().mockResolvedValue({
      on: jest.fn((event, callback) => {
        if (event === "data") callback(Buffer.from("test content"));
        if (event === "end") callback();
      }),
    }),
    putObject: jest.fn().mockResolvedValue({}),
  })),
}));

// Mock NATS
jest.mock("nats", () => ({
  connect: jest.fn(),
  StringCodec: jest.fn(),
}));

// Mock Redis
jest.mock("redis", () => ({
  createClient: jest.fn().mockReturnValue({
    connect: jest.fn(),
    disconnect: jest.fn(),
    get: jest.fn(),
    set: jest.fn(),
    del: jest.fn(),
  }),
}));

// Mock bcrypt
jest.mock("bcrypt", () => ({
  hash: jest.fn().mockResolvedValue("hashed_password"),
  compare: jest.fn().mockResolvedValue(true),
}));

// Create a proper mock database that implements the Drizzle query builder pattern
const createMockQuery = () => ({
  from: jest.fn().mockReturnThis(),
  where: jest.fn().mockReturnThis(),
  orderBy: jest.fn().mockReturnThis(),
  limit: jest.fn().mockReturnThis(),
  returning: jest
    .fn()
    .mockResolvedValue([
      { id: "test-id", createdAt: new Date(), updatedAt: new Date() },
    ]),
  then: jest.fn().mockResolvedValue([]),
});

// Mock database connection
jest.mock("@/database/connection", () => ({
  getDatabase: jest.fn().mockReturnValue({
    select: jest.fn().mockImplementation(() => createMockQuery()),
    insert: jest.fn().mockImplementation(() => ({
      values: jest.fn().mockImplementation(() => createMockQuery()),
    })),
    update: jest.fn().mockImplementation(() => ({
      set: jest.fn().mockImplementation(() => createMockQuery()),
    })),
    delete: jest.fn().mockImplementation(() => createMockQuery()),
  }),
  getTables: jest.fn().mockReturnValue({
    users: {
      id: "id",
      userId: "userId",
      email: "email",
      passwordHash: "passwordHash",
      createdAt: "createdAt",
      updatedAt: "updatedAt",
    },
    apiKeys: {
      id: "id",
      userId: "userId",
      keyValue: "keyValue",
      isActive: "isActive",
      createdAt: "createdAt",
      updatedAt: "updatedAt",
      lastUsedAt: "lastUsedAt",
    },
    customBots: {
      id: "id",
      userId: "userId",
      name: "name",
      version: "version",
      createdAt: "createdAt",
    },
    userBots: {
      id: "id",
      userId: "userId",
      customBotId: "customBotId",
      createdAt: "createdAt",
    },
    backtests: { id: "id", userId: "userId", createdAt: "createdAt" },
    botLogs: { id: "id", userId: "userId", createdAt: "createdAt" },
    botExecutions: { id: "id", userId: "userId", createdAt: "createdAt" },
    systemLogs: { id: "id", createdAt: "createdAt" },
  }),
  getDatabaseConfig: jest.fn().mockReturnValue({
    type: "sqlite",
    url: ":memory:",
  }),
}));
