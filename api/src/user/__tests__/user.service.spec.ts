import { ForbiddenException } from "@nestjs/common";
import * as bcrypt from "bcrypt";
import { AuthenticatedUser } from "@/auth/auth.types";
import { AdminMutationLockRepository } from "../admin-mutation-lock.repository";
import { USER_ROLES } from "../user.constants";
import { UserRepository } from "../user.repository";
import { UserService } from "../user.service";

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

describe("UserService", () => {
  let service: UserService;
  let users: jest.Mocked<UserRepository>;
  let adminMutationLock: jest.Mocked<AdminMutationLockRepository>;
  const originalAdminEmail = process.env.THE0_ADMIN_EMAIL;

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
    delete process.env.THE0_ADMIN_EMAIL;
    users = {
      findById: jest.fn(),
      countActiveAdmins: jest.fn(),
      update: jest.fn(),
      deactivate: jest.fn(),
      findByEmail: jest.fn(),
      findByUsername: jest.fn(),
      findAnotherByUsername: jest.fn(),
      list: jest.fn(),
      createUser: jest.fn(),
      updatePassword: jest.fn(),
      updateProfile: jest.fn(),
      updateLastLogin: jest.fn(),
      count: jest.fn(),
      hasActiveAdmin: jest.fn(),
      createFirstAdmin: jest.fn(),
      promoteToAdmin: jest.fn(),
    } as unknown as jest.Mocked<UserRepository>;
    adminMutationLock = {
      withLock: jest.fn((callback: () => Promise<unknown>) => callback()),
    } as unknown as jest.Mocked<AdminMutationLockRepository>;
    (bcrypt.compare as jest.Mock).mockResolvedValue(true);
    service = new UserService(users, adminMutationLock);
  });

  afterEach(() => {
    if (originalAdminEmail === undefined) {
      delete process.env.THE0_ADMIN_EMAIL;
    } else {
      process.env.THE0_ADMIN_EMAIL = originalAdminEmail;
    }
  });

  it("blocks self-demotion", async () => {
    const admin = makeUser();
    users.findById.mockResolvedValue(admin);

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
    users.findById.mockResolvedValue(admin);

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
    users.findById.mockResolvedValue(admin);
    users.countActiveAdmins.mockResolvedValue(1);

    await expect(
      service.updateUser(admin.id, { role: USER_ROLES.USER }, actor),
    ).rejects.toThrow("At least one active admin is required");
  });

  it("blocks deactivating the final active admin", async () => {
    const admin = makeUser();
    users.findById.mockResolvedValue(admin);
    users.countActiveAdmins.mockResolvedValue(1);

    await expect(
      service.updateUser(admin.id, { isActive: false }, actor),
    ).rejects.toThrow("At least one active admin is required");
  });

  it("allows demoting an admin when another active admin remains", async () => {
    const target = makeUser();
    const demoted = makeUser({ role: USER_ROLES.USER });
    users.findById.mockResolvedValue(target);
    users.countActiveAdmins.mockResolvedValue(2);
    users.update.mockResolvedValue(demoted);

    const result = await service.updateUser(
      target.id,
      { role: USER_ROLES.USER },
      actor,
    );

    expect(result.role).toBe(USER_ROLES.USER);
  });

  it("blocks deleting the final active admin account", async () => {
    const admin = makeUser();
    users.findById.mockResolvedValue(admin);
    users.countActiveAdmins.mockResolvedValue(1);

    await expect(
      service.deleteAccount({ ...actor, id: admin.id }, "password"),
    ).rejects.toThrow("At least one active admin is required");
  });

  it("blocks admin self password reset", async () => {
    await expect(
      service.resetPassword(actor.id, "new-password", actor),
    ).rejects.toThrow("Use change password for your own admin account");
    expect(users.updatePassword).not.toHaveBeenCalled();
  });

  it("blocks changing the configured root admin email", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    const admin = makeUser({ email: "admin@example.com" });
    users.findById.mockResolvedValue(admin);

    await expect(
      service.updateUser(admin.id, { email: "new-admin@example.com" }, actor),
    ).rejects.toThrow(
      "Configured root admin email is managed by deployment configuration",
    );
    expect(users.update).not.toHaveBeenCalled();
  });

  it("blocks admin password reset for the configured root admin", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    const admin = makeUser({ email: "admin@example.com" });
    users.findById.mockResolvedValue(admin);

    await expect(
      service.resetPassword(admin.id, "new-password", actor),
    ).rejects.toThrow(
      "Configured root admin password is managed by deployment configuration",
    );
    expect(users.updatePassword).not.toHaveBeenCalled();
  });

  it("blocks self-service password change for the configured root admin", async () => {
    process.env.THE0_ADMIN_EMAIL = "admin@example.com";
    const admin = makeUser({ id: actor.id, email: "admin@example.com" });
    users.findById.mockResolvedValue(admin);

    await expect(
      service.changePassword(actor, "current-password", "new-password"),
    ).rejects.toThrow(
      "Configured root admin password is managed by deployment configuration",
    );
    expect(bcrypt.compare).not.toHaveBeenCalled();
    expect(users.updatePassword).not.toHaveBeenCalled();
  });
});
