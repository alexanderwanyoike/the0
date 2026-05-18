import * as bcrypt from "bcrypt";
import { Logger } from "@nestjs/common";
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
      promoteToAdmin: jest.fn(),
      promoteToAdminAndSetPassword: jest.fn(),
      updatePassword: jest.fn(),
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
    expect(loggerWarn).toHaveBeenCalledWith(
      "THE0_ADMIN_PASSWORD is configured. Remove it after the admin password has been applied.",
    );
  });

  it("does not auto-create on a fresh database without a complete config", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    users.count.mockResolvedValue(0);

    await service.onModuleInit();

    expect(setupLocks.withLock).not.toHaveBeenCalled();
    expect(users.createFirstAdmin).not.toHaveBeenCalled();
  });

  it("blocks configured bootstrap when the password fails API policy", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    process.env.THE0_ADMIN_PASSWORD = "short";
    users.count.mockResolvedValue(0);

    await service.onModuleInit();

    expect(setupLocks.withLock).not.toHaveBeenCalled();
    expect(users.createFirstAdmin).not.toHaveBeenCalled();
    expect(loggerWarn).toHaveBeenCalledWith(
      "THE0_ADMIN_PASSWORD is invalid: Password must be at least 6 characters long",
    );
  });

  it("promotes a matching active user and sets their password when no admin exists", async () => {
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

  it("requires a password before promoting a configured email", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    users.count.mockResolvedValue(1);
    users.list.mockResolvedValue([
      user({ id: "target", email: "admin@example.com" }),
    ]);

    await service.onModuleInit();

    expect(users.promoteToAdminAndSetPassword).not.toHaveBeenCalled();
    expect(loggerWarn).toHaveBeenCalledWith(
      "No admin configured. Set THE0_ADMIN_PASSWORD with THE0_ADMIN_EMAIL to promote the configured active user",
    );
  });

  it("updates an existing matching admin password once", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    (bcrypt.compare as jest.Mock).mockResolvedValueOnce(false);
    users.count.mockResolvedValue(1);
    users.list.mockResolvedValue([
      user({
        id: "admin-id",
        email: "admin@example.com",
        role: USER_ROLES.ADMIN,
      }),
    ]);

    await service.onModuleInit();

    expect(users.updatePassword).toHaveBeenCalledWith(
      "admin-id",
      "hashed_password",
    );
  });

  it("updates an existing matching admin when the current password hash is missing", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    users.count.mockResolvedValue(1);
    users.list.mockResolvedValue([
      user({
        id: "admin-id",
        email: "admin@example.com",
        passwordHash: "",
        role: USER_ROLES.ADMIN,
      }),
    ]);

    await service.onModuleInit();

    expect(bcrypt.compare).not.toHaveBeenCalled();
    expect(users.updatePassword).toHaveBeenCalledWith(
      "admin-id",
      "hashed_password",
    );
  });

  it("does not update an existing matching admin when the password already matches", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    (bcrypt.compare as jest.Mock).mockResolvedValueOnce(true);
    users.count.mockResolvedValue(1);
    users.list.mockResolvedValue([
      user({
        id: "admin-id",
        email: "admin@example.com",
        role: USER_ROLES.ADMIN,
      }),
    ]);

    await service.onModuleInit();

    expect(users.updatePassword).not.toHaveBeenCalled();
    expect(users.promoteToAdminAndSetPassword).not.toHaveBeenCalled();
  });

  it("warns and does not mutate when configured email points to a non-admin", async () => {
    process.env.THE0_ADMIN_EMAIL = "user@example.com";
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    users.count.mockResolvedValue(1);
    users.list.mockResolvedValue([
      user({
        id: "admin-id",
        email: "admin@example.com",
        role: USER_ROLES.ADMIN,
      }),
      user({ id: "user-id", email: "user@example.com", role: USER_ROLES.USER }),
    ]);

    await service.onModuleInit();

    expect(users.updatePassword).not.toHaveBeenCalled();
    expect(users.promoteToAdminAndSetPassword).not.toHaveBeenCalled();
    expect(loggerWarn).toHaveBeenCalledWith(
      "Configured admin email does not match an active admin; skipping password update",
    );
  });

  it("preserves metadata admin users before configured bootstrap", async () => {
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
      ]);

    await service.onModuleInit();

    expect(users.promoteToAdmin).toHaveBeenCalledWith("legacy-admin");
  });

  it("does not promote inactive or unmatched configured users", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    process.env.THE0_ADMIN_PASSWORD = "secret123";
    users.count.mockResolvedValue(1);
    users.list.mockResolvedValue([
      user({ id: "inactive", email: "admin@example.com", isActive: false }),
    ]);

    await service.onModuleInit();

    expect(users.promoteToAdminAndSetPassword).not.toHaveBeenCalled();
    expect(loggerWarn).toHaveBeenCalledWith(
      "Configured admin email does not match an active user; no admin was promoted",
    );
  });
});
