import { Logger } from "@nestjs/common";
import * as bcrypt from "bcrypt";
import { AdminMutationLockRepository } from "@/user/admin-mutation-lock.repository";
import { USER_ROLES } from "@/user/user.constants";
import { UserRepository } from "@/user/user.repository";
import { UserRecord } from "@/user/user.types";
import { AdminBootstrapService } from "../admin-bootstrap.service";
import { SetupLockRepository } from "../setup-lock.repository";

jest.mock("bcrypt", () => ({
  compare: jest.fn(),
}));

jest.mock("@/common/password", () => ({
  hashPassword: jest.fn().mockResolvedValue("hashed_password"),
}));

jest.mock("@/common/password-policy", () => ({
  validatePasswordPolicy: jest.fn((password: string) =>
    password.length < 6 ? "Password must be at least 6 characters long" : null,
  ),
}));

describe("AdminBootstrapService", () => {
  let users: jest.Mocked<UserRepository>;
  let setupLocks: jest.Mocked<SetupLockRepository>;
  let adminMutationLocks: jest.Mocked<AdminMutationLockRepository>;
  let service: AdminBootstrapService;
  let loggerWarn: jest.SpyInstance;
  let loggerLog: jest.SpyInstance;

  const originalAdminEmail = process.env.THE0_ADMIN_EMAIL;
  const originalAdminPassword = process.env.THE0_ADMIN_PASSWORD;

  function user(overrides: Partial<UserRecord> = {}): UserRecord {
    return {
      id: overrides.id ?? "user-id",
      username: overrides.username ?? "user",
      email: overrides.email ?? "user@example.com",
      passwordHash: overrides.passwordHash ?? "existing-hash",
      firstName: overrides.firstName ?? null,
      lastName: overrides.lastName ?? null,
      role: overrides.role ?? USER_ROLES.USER,
      sessionVersion: overrides.sessionVersion ?? 0,
      isActive: overrides.isActive ?? true,
      isEmailVerified: overrides.isEmailVerified ?? true,
      lastLoginAt: overrides.lastLoginAt ?? null,
      metadata: overrides.metadata ?? {},
      createdAt: overrides.createdAt ?? new Date("2026-05-16T00:00:00Z"),
      updatedAt: overrides.updatedAt ?? new Date("2026-05-16T00:00:00Z"),
    };
  }

  beforeEach(() => {
    users = {
      count: jest.fn(),
      list: jest.fn(),
      createFirstAdmin: jest.fn(),
      createUser: jest.fn(),
      promoteToAdmin: jest.fn(),
      promoteToAdminAndSetPassword: jest.fn(),
      updatePassword: jest.fn(),
      update: jest.fn(),
    } as unknown as jest.Mocked<UserRepository>;
    setupLocks = {
      withLock: jest.fn(async (callback) => ({
        acquired: true,
        value: await callback(),
      })),
    } as unknown as jest.Mocked<SetupLockRepository>;
    adminMutationLocks = {
      withLock: jest.fn(async (callback) => callback()),
    } as unknown as jest.Mocked<AdminMutationLockRepository>;

    loggerWarn = jest.spyOn(Logger.prototype, "warn").mockImplementation();
    loggerLog = jest.spyOn(Logger.prototype, "log").mockImplementation();
    (bcrypt.compare as jest.Mock).mockResolvedValue(false);

    delete process.env.THE0_ADMIN_EMAIL;
    delete process.env.THE0_ADMIN_PASSWORD;

    service = new AdminBootstrapService(users, setupLocks, adminMutationLocks);
  });

  afterEach(() => {
    if (originalAdminEmail === undefined) {
      delete process.env.THE0_ADMIN_EMAIL;
    } else {
      process.env.THE0_ADMIN_EMAIL = originalAdminEmail;
    }
    if (originalAdminPassword === undefined) {
      delete process.env.THE0_ADMIN_PASSWORD;
    } else {
      process.env.THE0_ADMIN_PASSWORD = originalAdminPassword;
    }
    loggerWarn.mockRestore();
    loggerLog.mockRestore();
    jest.clearAllMocks();
  });

  it("auto-creates a configured admin on a fresh database", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    users.count.mockResolvedValueOnce(0).mockResolvedValueOnce(0);
    users.createFirstAdmin.mockResolvedValue(user({ role: USER_ROLES.ADMIN }));

    await service.onModuleInit();

    expect(setupLocks.withLock).toHaveBeenCalledTimes(1);
    expect(users.createFirstAdmin).toHaveBeenCalledWith(
      { username: "admin", email: "admin@example.com" },
      "hashed_password",
    );
    expect(loggerLog).toHaveBeenCalledWith("Created configured first admin user");
  });

  it("fails startup when admin email is missing", async () => {
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    users.count.mockResolvedValue(0);

    await expect(service.onModuleInit()).rejects.toThrow(
      "THE0_ADMIN_EMAIL must be configured",
    );
    expect(setupLocks.withLock).not.toHaveBeenCalled();
  });

  it("fails startup when admin password is missing", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    users.count.mockResolvedValue(0);

    await expect(service.onModuleInit()).rejects.toThrow(
      "THE0_ADMIN_PASSWORD must be configured",
    );

    expect(setupLocks.withLock).not.toHaveBeenCalled();
    expect(users.createFirstAdmin).not.toHaveBeenCalled();
  });

  it("fails startup when admin email is invalid", async () => {
    process.env.THE0_ADMIN_EMAIL = "not-an-email";
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    users.count.mockResolvedValue(0);

    await expect(service.onModuleInit()).rejects.toThrow(
      "THE0_ADMIN_EMAIL must be a valid email address",
    );

    expect(setupLocks.withLock).not.toHaveBeenCalled();
    expect(users.createFirstAdmin).not.toHaveBeenCalled();
  });

  it("fails startup when the password fails API policy", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    process.env.THE0_ADMIN_PASSWORD = "short";
    users.count.mockResolvedValue(0);

    await expect(service.onModuleInit()).rejects.toThrow(
      "THE0_ADMIN_PASSWORD is invalid: Password must be at least 6 characters long",
    );

    expect(setupLocks.withLock).not.toHaveBeenCalled();
    expect(users.createFirstAdmin).not.toHaveBeenCalled();
  });

  it("promotes a matching user and sets their password", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    const matchingUser = user({ id: "target", email: "admin@example.com" });
    users.count.mockResolvedValue(1);
    users.list.mockResolvedValue([matchingUser]);

    await service.onModuleInit();

    expect(adminMutationLocks.withLock).toHaveBeenCalledTimes(1);
    expect(users.promoteToAdminAndSetPassword).toHaveBeenCalledWith(
      "target",
      "hashed_password",
    );
    expect(users.promoteToAdmin).not.toHaveBeenCalled();
  });

  it("matches the configured root admin email case-insensitively", async () => {
    process.env.THE0_ADMIN_EMAIL = "Admin@Example.COM";
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    users.count.mockResolvedValue(1);
    users.list.mockResolvedValue([
      user({ id: "target", email: "admin@example.com" }),
    ]);

    await service.onModuleInit();

    expect(users.promoteToAdminAndSetPassword).toHaveBeenCalledWith(
      "target",
      "hashed_password",
    );
    expect(users.createUser).not.toHaveBeenCalled();
  });

  it("does not update the configured root admin password when it already matches", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    (bcrypt.compare as jest.Mock).mockResolvedValue(true);
    users.count.mockResolvedValue(1);
    users.list.mockResolvedValue([
      user({ id: "admin", email: "admin@example.com", role: USER_ROLES.ADMIN }),
    ]);

    await service.onModuleInit();

    expect(users.promoteToAdminAndSetPassword).not.toHaveBeenCalled();
    expect(users.update).not.toHaveBeenCalled();
  });

  it("updates the configured root admin password when config changes", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    (bcrypt.compare as jest.Mock).mockResolvedValue(false);
    users.count.mockResolvedValue(1);
    users.list.mockResolvedValue([
      user({
        id: "admin-id",
        email: "admin@example.com",
        role: USER_ROLES.ADMIN,
      }),
    ]);

    await service.onModuleInit();

    expect(users.promoteToAdminAndSetPassword).toHaveBeenCalledWith(
      "admin-id",
      "hashed_password",
    );
  });

  it("creates the configured root admin when users exist but the configured email is absent", async () => {
    process.env.THE0_ADMIN_EMAIL = "root@example.com";
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    users.count.mockResolvedValue(1);
    users.list.mockResolvedValue([
      user({
        id: "admin-id",
        email: "admin@example.com",
        role: USER_ROLES.ADMIN,
      }),
      user({ id: "user-id", username: "root", email: "user@example.com" }),
    ]);
    users.createUser.mockResolvedValue(
      user({ id: "root-id", email: "root@example.com", role: USER_ROLES.ADMIN }),
    );

    await service.onModuleInit();

    expect(users.createUser).toHaveBeenCalledWith(
      {
        email: "root@example.com",
        password: "",
        role: USER_ROLES.ADMIN,
        isActive: true,
      },
      "root-admin",
      "root@example.com",
      "hashed_password",
    );
  });

  it("preserves metadata admin users before configured bootstrap", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    users.count.mockResolvedValue(1);
    users.list
      .mockResolvedValueOnce([
        user({
          id: "legacy-admin",
          role: USER_ROLES.USER,
          metadata: { role: USER_ROLES.ADMIN },
        }),
      ])
      .mockResolvedValueOnce([
        user({ id: "legacy-admin", role: USER_ROLES.ADMIN }),
        user({ id: "configured", email: "admin@example.com" }),
      ]);

    await service.onModuleInit();

    expect(users.promoteToAdmin).toHaveBeenCalledWith("legacy-admin");
    expect(users.promoteToAdminAndSetPassword).toHaveBeenCalledWith(
      "configured",
      "hashed_password",
    );
  });

  it("reactivates and promotes the configured root admin when the password matches", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    (bcrypt.compare as jest.Mock).mockResolvedValue(true);
    users.count.mockResolvedValue(1);
    users.list.mockResolvedValue([
      user({
        id: "configured",
        email: "admin@example.com",
        role: USER_ROLES.USER,
        isActive: false,
      }),
    ]);

    await service.onModuleInit();

    expect(users.promoteToAdminAndSetPassword).not.toHaveBeenCalled();
    expect(users.update).toHaveBeenCalledWith("configured", {
      role: USER_ROLES.ADMIN,
      isActive: true,
    });
  });

  it("sets password for the configured root admin when the existing hash is missing", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    users.count.mockResolvedValue(1);
    users.list.mockResolvedValue([
      user({
        id: "configured",
        email: "admin@example.com",
        role: USER_ROLES.ADMIN,
        passwordHash: "",
      }),
    ]);

    await service.onModuleInit();

    expect(bcrypt.compare).not.toHaveBeenCalled();
    expect(users.promoteToAdminAndSetPassword).toHaveBeenCalledWith(
      "configured",
      "hashed_password",
    );
  });
});
