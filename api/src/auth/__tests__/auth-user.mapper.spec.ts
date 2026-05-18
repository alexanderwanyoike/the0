import { USER_ROLES } from "@/user/user.constants";
import { UserRecord } from "@/user/user.types";
import { toAuthUser } from "../auth-user.mapper";

describe("toAuthUser", () => {
  const originalAdminEmail = process.env.THE0_ADMIN_EMAIL;

  function user(overrides: Partial<UserRecord> = {}): UserRecord {
    return {
      id: overrides.id ?? "user-id",
      username: overrides.username ?? "user",
      email: overrides.email ?? "user@example.com",
      passwordHash: overrides.passwordHash ?? "hash",
      firstName: overrides.firstName ?? null,
      lastName: overrides.lastName ?? null,
      role: overrides.role ?? USER_ROLES.ADMIN,
      sessionVersion: overrides.sessionVersion ?? 0,
      isActive: overrides.isActive ?? true,
      isEmailVerified: overrides.isEmailVerified ?? true,
      lastLoginAt: overrides.lastLoginAt ?? null,
      metadata: overrides.metadata ?? {},
      createdAt: overrides.createdAt ?? new Date("2026-05-16T00:00:00Z"),
      updatedAt: overrides.updatedAt ?? new Date("2026-05-16T00:00:00Z"),
    };
  }

  afterEach(() => {
    if (originalAdminEmail === undefined) {
      delete process.env.THE0_ADMIN_EMAIL;
    } else {
      process.env.THE0_ADMIN_EMAIL = originalAdminEmail;
    }
  });

  it("marks the configured root admin case-insensitively", () => {
    process.env.THE0_ADMIN_EMAIL = "Admin@Example.COM";

    expect(
      toAuthUser(user({ email: "admin@example.com" })).isConfiguredRootAdmin,
    ).toBe(true);
  });
});
