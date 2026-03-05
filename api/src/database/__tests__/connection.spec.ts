// Unmock the global bootstrap mock for database/connection
jest.unmock("@/database/connection");
jest.unmock("../connection");

const mockPostgres = jest.fn().mockReturnValue({});

jest.mock("postgres", () => {
  return { __esModule: true, default: mockPostgres };
});

jest.mock("drizzle-orm/postgres-js", () => ({
  drizzle: jest.fn().mockReturnValue({}),
}));

jest.mock("drizzle-orm/better-sqlite3", () => ({
  drizzle: jest.fn().mockReturnValue({}),
}));

jest.mock("better-sqlite3", () => {
  return { __esModule: true, default: jest.fn().mockReturnValue({}) };
});

jest.mock("../schema/users", () => ({
  usersTable: {},
  usersTableSqlite: {},
  apiKeysTable: {},
  apiKeysTableSqlite: {},
}));

jest.mock("../schema/custom-bots", () => ({
  customBotsTable: {},
  customBotsTableSqlite: {},
}));

jest.mock("../schema/bots", () => ({
  botsTable: {},
  botsTableSqlite: {},
}));

describe("connection", () => {
  beforeEach(() => {
    mockPostgres.mockClear();
    // Reset the singleton by clearing the module's internal state
    jest.resetModules();
  });

  it("should pass pool config to postgres client", () => {
    jest.doMock("../../config/database.config", () => ({
      loadConfig: jest.fn().mockReturnValue({
        type: "postgresql",
        url: "postgres://user:pass@localhost:5432/testdb",
        host: "localhost",
        port: 5432,
        username: "user",
        password: "pass",
        database: "testdb",
        pool: {
          max: 5,
          idleTimeout: 30,
          connectTimeout: 15,
          maxLifetime: 900,
        },
      }),
    }));

    // Re-mock postgres after resetModules
    jest.doMock("postgres", () => {
      return { __esModule: true, default: mockPostgres };
    });
    jest.doMock("drizzle-orm/postgres-js", () => ({
      drizzle: jest.fn().mockReturnValue({}),
    }));
    jest.doMock("drizzle-orm/better-sqlite3", () => ({
      drizzle: jest.fn().mockReturnValue({}),
    }));
    jest.doMock("better-sqlite3", () => {
      return { __esModule: true, default: jest.fn().mockReturnValue({}) };
    });
    jest.doMock("../schema/users", () => ({
      usersTable: {},
      usersTableSqlite: {},
      apiKeysTable: {},
      apiKeysTableSqlite: {},
    }));
    jest.doMock("../schema/custom-bots", () => ({
      customBotsTable: {},
      customBotsTableSqlite: {},
    }));
    jest.doMock("../schema/bots", () => ({
      botsTable: {},
      botsTableSqlite: {},
    }));

    const { getDatabase } = require("../connection");
    getDatabase();

    expect(mockPostgres).toHaveBeenCalledWith(
      "postgres://user:pass@localhost:5432/testdb",
      expect.objectContaining({
        max: 5,
        idle_timeout: 30,
        connect_timeout: 15,
        max_lifetime: 900,
      }),
    );
  });

  it("should use default pool config values", () => {
    jest.doMock("../../config/database.config", () => ({
      loadConfig: jest.fn().mockReturnValue({
        type: "postgresql",
        url: "postgres://user:pass@localhost:5432/testdb",
        host: "localhost",
        port: 5432,
        username: "user",
        password: "pass",
        database: "testdb",
        pool: {
          max: 10,
          idleTimeout: 20,
          connectTimeout: 10,
          maxLifetime: 1800,
        },
      }),
    }));

    jest.doMock("postgres", () => {
      return { __esModule: true, default: mockPostgres };
    });
    jest.doMock("drizzle-orm/postgres-js", () => ({
      drizzle: jest.fn().mockReturnValue({}),
    }));
    jest.doMock("drizzle-orm/better-sqlite3", () => ({
      drizzle: jest.fn().mockReturnValue({}),
    }));
    jest.doMock("better-sqlite3", () => {
      return { __esModule: true, default: jest.fn().mockReturnValue({}) };
    });
    jest.doMock("../schema/users", () => ({
      usersTable: {},
      usersTableSqlite: {},
      apiKeysTable: {},
      apiKeysTableSqlite: {},
    }));
    jest.doMock("../schema/custom-bots", () => ({
      customBotsTable: {},
      customBotsTableSqlite: {},
    }));
    jest.doMock("../schema/bots", () => ({
      botsTable: {},
      botsTableSqlite: {},
    }));

    const { getDatabase } = require("../connection");
    getDatabase();

    expect(mockPostgres).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({
        max: 10,
        idle_timeout: 20,
        connect_timeout: 10,
        max_lifetime: 1800,
      }),
    );
  });
});
