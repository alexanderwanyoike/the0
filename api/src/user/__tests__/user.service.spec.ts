import { ForbiddenException } from "@nestjs/common";
import * as bcrypt from "bcrypt";
import { getDatabase, getDatabaseConfig } from "@/database/connection";
import { AuthenticatedUser } from "@/auth/auth.types";
import { USER_ROLES } from "../user.constants";
import { UserService } from "../user.service";

jest.mock("@/database/connection", () => ({
  getDatabase: jest.fn(),
  getDatabaseConfig: jest.fn(),
}));

jest.mock("bcrypt", () => ({
  compare: jest.fn(),
  hash: jest.fn(),
}));

interface TestUser {
  id: string;
  username: string;
  email: string;
  passwordHash: string;
  firstName: string | null;
  lastName: string | null;
  role: string;
  sessionVersion: number;
  isActive: boolean;
  isEmailVerified: boolean;
  lastLoginAt: Date | null;
  metadata: Record<string, unknown>;
  createdAt: Date;
  updatedAt: Date;
}

function makeUser(overrides: Partial<TestUser> = {}): TestUser {
  const now = new Date("2026-05-16T00:00:00Z");
  return {
    id: "admin-1",
    username: "admin",
    email: "admin@example.com",
    passwordHash: "hashed-password",
    firstName: null,
    lastName: null,
    role: USER_ROLES.ADMIN,
    sessionVersion: 0,
    isActive: true,
    isEmailVerified: true,
    lastLoginAt: null,
    metadata: {},
    createdAt: now,
    updatedAt: now,
    ...overrides,
  };
}

function createMockDb() {
  const whereResults: unknown[][] = [];
  const allResults: unknown[][] = [];
  const updateResults: TestUser[][] = [];
  const insertValues = jest.fn();
  const updateSet = jest.fn();
  const deleteWhere = jest.fn().mockResolvedValue(undefined);

  const db = {
    select: jest.fn(() => ({
      from: jest.fn(() => ({
        where: jest.fn(() => Promise.resolve(whereResults.shift() ?? [])),
        then: (
          resolve: (value: TestUser[]) => unknown,
          reject: (reason: unknown) => unknown,
        ) => Promise.resolve(allResults.shift() ?? []).then(resolve, reject),
      })),
    })),
    update: jest.fn(() => ({
      set: updateSet.mockImplementation(() => ({
        where: jest.fn(() => ({
          returning: jest.fn(() =>
            Promise.resolve(updateResults.shift() ?? []),
          ),
        })),
      })),
    })),
    insert: jest.fn(() => ({
      values: insertValues.mockResolvedValue(undefined),
    })),
    delete: jest.fn(() => ({
      where: deleteWhere,
    })),
  };

  return {
    db,
    whereResults,
    allResults,
    updateResults,
    insertValues,
    updateSet,
    deleteWhere,
  };
}

describe("UserService", () => {
  let service: UserService;
  let mockDb: ReturnType<typeof createMockDb>;

  const actor: AuthenticatedUser = {
    uid: "actor-admin",
    id: "actor-admin",
    username: "actor",
    email: "actor@example.com",
    firstName: null,
    lastName: null,
    role: USER_ROLES.ADMIN,
    isActive: true,
    isEmailVerified: true,
    authType: "jwt",
  };

  beforeEach(() => {
    jest.clearAllMocks();
    mockDb = createMockDb();
    (getDatabase as jest.Mock).mockReturnValue(mockDb.db);
    (getDatabaseConfig as jest.Mock).mockReturnValue({ type: "sqlite" });
    (bcrypt.compare as jest.Mock).mockResolvedValue(true);
    service = new UserService();
  });

  it("blocks self-demotion", async () => {
    const admin = makeUser();
    mockDb.whereResults.push([admin]);

    await expect(
      service.updateUser(
        admin.id,
        { role: USER_ROLES.USER },
        {
          ...actor,
          id: admin.id,
        },
      ),
    ).rejects.toThrow(ForbiddenException);
  });

  it("blocks self-deactivation", async () => {
    const admin = makeUser();
    mockDb.whereResults.push([admin]);

    await expect(
      service.updateUser(
        admin.id,
        { isActive: false },
        {
          ...actor,
          id: admin.id,
        },
      ),
    ).rejects.toThrow(ForbiddenException);
  });

  it("blocks demoting the final active admin", async () => {
    const admin = makeUser();
    mockDb.whereResults.push([admin]);
    mockDb.whereResults.push([{ value: 1 }]);

    await expect(
      service.updateUser(admin.id, { role: USER_ROLES.USER }, actor),
    ).rejects.toThrow("At least one active admin is required");
  });

  it("blocks deactivating the final active admin", async () => {
    const admin = makeUser();
    mockDb.whereResults.push([admin]);
    mockDb.whereResults.push([{ value: 1 }]);

    await expect(
      service.updateUser(admin.id, { isActive: false }, actor),
    ).rejects.toThrow("At least one active admin is required");
  });

  it("allows demoting an admin when another active admin remains", async () => {
    const target = makeUser();
    const remainingAdmin = makeUser({
      id: "admin-2",
      username: "admin2",
      email: "admin2@example.com",
    });
    const demoted = makeUser({ role: USER_ROLES.USER });
    mockDb.whereResults.push([target]);
    mockDb.whereResults.push([{ value: 2 }]);
    mockDb.updateResults.push([demoted]);

    const result = await service.updateUser(
      target.id,
      { role: USER_ROLES.USER },
      actor,
    );

    expect(result.role).toBe(USER_ROLES.USER);
  });

  it("blocks deleting the final active admin account", async () => {
    const admin = makeUser();
    mockDb.whereResults.push([admin]);
    mockDb.whereResults.push([{ value: 1 }]);

    await expect(
      service.deleteAccount({ ...actor, id: admin.id }, "password"),
    ).rejects.toThrow("At least one active admin is required");
  });
});
