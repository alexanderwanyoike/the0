import {
  BadRequestException,
  ForbiddenException,
  Injectable,
  NotFoundException,
  ServiceUnavailableException,
  UnauthorizedException,
} from "@nestjs/common";
import { and, count, eq, ne, sql, type AnyColumn } from "drizzle-orm";
import * as bcrypt from "bcrypt";
import { hashPassword } from "@/common/password";
import { getDatabase, getDatabaseConfig } from "@/database/connection";
import {
  type AdminMutationLock,
  adminMutationLocksTable,
  adminMutationLocksTableSqlite,
  usersTable,
  usersTableSqlite,
} from "@/database/schema/users";
import { AuthenticatedUser } from "@/auth/auth.types";
import { USER_ROLES, UserRole } from "./user.constants";
import {
  CreateAdminUserInput,
  SerializedUser,
  UpdateAdminUserInput,
  UpdateProfileInput,
  UserRecord,
} from "./user.types";

const ADMIN_MUTATION_LOCK_ID = "admin-users";
const ADMIN_MUTATION_LOCK_STALE_MS = 5 * 60 * 1000;

@Injectable()
export class UserService {
  private adminMutationLock = Promise.resolve();

  private getUserTable() {
    const config = getDatabaseConfig();
    return config.type === "sqlite" ? usersTableSqlite : usersTable;
  }

  private getAdminMutationLockTable() {
    const config = getDatabaseConfig();
    return config.type === "sqlite"
      ? adminMutationLocksTableSqlite
      : adminMutationLocksTable;
  }

  private serializeUser(user: UserRecord): SerializedUser {
    return {
      id: user.id,
      username: user.username,
      email: user.email,
      firstName: user.firstName,
      lastName: user.lastName,
      role: user.role === USER_ROLES.ADMIN ? USER_ROLES.ADMIN : USER_ROLES.USER,
      isActive: Boolean(user.isActive),
      isEmailVerified: Boolean(user.isEmailVerified),
      lastLoginAt: user.lastLoginAt,
      createdAt: user.createdAt,
      updatedAt: user.updatedAt,
    };
  }

  private async withAdminMutationLock<T>(
    callback: () => Promise<T>,
  ): Promise<T> {
    const previousLock = this.adminMutationLock;
    let releaseLock: () => void = () => undefined;
    this.adminMutationLock = new Promise<void>((resolve) => {
      releaseLock = resolve;
    });

    await previousLock;
    try {
      await this.acquireAdminMutationLock();
      try {
        return await callback();
      } finally {
        await this.releaseAdminMutationLock().catch((): void => undefined);
      }
    } finally {
      releaseLock();
    }
  }

  private async acquireAdminMutationLock(): Promise<void> {
    const db = getDatabase();
    const lockTable = this.getAdminMutationLockTable();

    for (let attempt = 0; attempt < 50; attempt += 1) {
      try {
        await db
          .insert(lockTable)
          .values({ id: ADMIN_MUTATION_LOCK_ID, lockedAt: Date.now() });
        return;
      } catch {
        const locks = (await db
          .select()
          .from(lockTable)
          .where(
            eq(lockTable.id, ADMIN_MUTATION_LOCK_ID),
          )) as AdminMutationLock[];
        const existingLock = locks[0];
        if (
          existingLock &&
          Date.now() - Number(existingLock.lockedAt) >
            ADMIN_MUTATION_LOCK_STALE_MS
        ) {
          await this.releaseAdminMutationLock();
          continue;
        }

        await wait(50);
      }
    }

    throw new ServiceUnavailableException(
      "Admin user management is temporarily locked",
    );
  }

  private async releaseAdminMutationLock(): Promise<void> {
    const db = getDatabase();
    const lockTable = this.getAdminMutationLockTable();
    await db.delete(lockTable).where(eq(lockTable.id, ADMIN_MUTATION_LOCK_ID));
  }

  private defaultUsername(email: string): string {
    return email.split("@")[0] || email;
  }

  private normalizeUsername(username: string | undefined, email: string) {
    const candidate = username?.trim() || this.defaultUsername(email);
    return candidate.trim();
  }

  async listUsers() {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const users = (await db.select().from(userTable)) as UserRecord[];
    return users.map((user) => this.serializeUser(user));
  }

  async createUser(input: CreateAdminUserInput) {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const email = input.email.trim();
    const username = this.normalizeUsername(input.username, email);

    const existingByEmail = await db
      .select()
      .from(userTable)
      .where(eq(userTable.email, email));
    if (existingByEmail.length > 0) {
      throw new BadRequestException("Email is already in use");
    }

    const existingByUsername = await db
      .select()
      .from(userTable)
      .where(eq(userTable.username, username));
    if (existingByUsername.length > 0) {
      throw new BadRequestException("Username is already in use");
    }

    const passwordHash = await hashPassword(input.password);
    const users = (await db
      .insert(userTable)
      .values({
        username,
        email,
        passwordHash,
        firstName: input.firstName,
        lastName: input.lastName,
        role: input.role || USER_ROLES.USER,
        isActive: input.isActive ?? true,
        isEmailVerified: true,
      })
      .returning()) as UserRecord[];

    return this.serializeUser(users[0]);
  }

  async updateUser(
    id: string,
    input: UpdateAdminUserInput,
    actor: AuthenticatedUser,
  ) {
    return this.withAdminMutationLock(() =>
      this.updateUserWithAdminMutationLock(id, input, actor),
    );
  }

  private async updateUserWithAdminMutationLock(
    id: string,
    input: UpdateAdminUserInput,
    actor: AuthenticatedUser,
  ) {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const user = await this.requireUser(id);

    if (id === actor.id && input.role && input.role !== USER_ROLES.ADMIN) {
      throw new ForbiddenException("You cannot demote your own admin account");
    }
    if (id === actor.id && input.isActive === false) {
      throw new ForbiddenException(
        "You cannot deactivate your own admin account",
      );
    }

    await this.assertAdminWillRemain(user, input);

    const nextEmail = input.email?.trim();
    const nextUsername = input.username?.trim();

    if (nextEmail && nextEmail !== user.email) {
      const existing = await db
        .select()
        .from(userTable)
        .where(eq(userTable.email, nextEmail));
      if (existing.length > 0) {
        throw new BadRequestException("Email is already in use");
      }
    }

    if (nextUsername && nextUsername !== user.username) {
      const existing = await db
        .select()
        .from(userTable)
        .where(eq(userTable.username, nextUsername));
      if (existing.length > 0) {
        throw new BadRequestException("Username is already in use");
      }
    }

    const updates: Record<string, unknown> = {
      updatedAt: new Date(),
    };
    if (nextUsername !== undefined) {
      updates.username = nextUsername;
    }
    if (nextEmail !== undefined) {
      updates.email = nextEmail;
    }
    if (input.firstName !== undefined) {
      updates.firstName = input.firstName;
    }
    if (input.lastName !== undefined) {
      updates.lastName = input.lastName;
    }
    if (input.role !== undefined) {
      updates.role = input.role;
    }
    if (input.isActive !== undefined) {
      updates.isActive = input.isActive;
    }

    const updated = (await db
      .update(userTable)
      .set(updates)
      .where(eq(userTable.id, id))
      .returning()) as UserRecord[];

    return this.serializeUser(updated[0]);
  }

  async resetPassword(id: string, password: string) {
    const db = getDatabase();
    const userTable = this.getUserTable();
    await this.requireUser(id);

    const passwordHash = await hashPassword(password);
    const updated = (await db
      .update(userTable)
      .set({
        passwordHash,
        sessionVersion: sqlIncrement(userTable.sessionVersion),
        updatedAt: new Date(),
      })
      .where(eq(userTable.id, id))
      .returning()) as UserRecord[];

    return this.serializeUser(updated[0]);
  }

  async updateProfile(actor: AuthenticatedUser, input: UpdateProfileInput) {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const updates: Record<string, unknown> = { updatedAt: new Date() };

    const nextUsername = input.username?.trim();

    if (nextUsername) {
      const existing = await db
        .select()
        .from(userTable)
        .where(
          and(eq(userTable.username, nextUsername), ne(userTable.id, actor.id)),
        );
      if (existing.length > 0) {
        throw new BadRequestException("Username is already in use");
      }
      updates.username = nextUsername;
    }
    if (input.firstName !== undefined) updates.firstName = input.firstName;
    if (input.lastName !== undefined) updates.lastName = input.lastName;

    const updated = (await db
      .update(userTable)
      .set(updates)
      .where(eq(userTable.id, actor.id))
      .returning()) as UserRecord[];

    return this.serializeUser(updated[0]);
  }

  async changePassword(
    actor: AuthenticatedUser,
    currentPassword: string,
    newPassword: string,
  ) {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const user = await this.requireUser(actor.id);

    const passwordMatches = await bcrypt.compare(
      currentPassword,
      user.passwordHash,
    );
    if (!passwordMatches) {
      throw new UnauthorizedException("Current password is incorrect");
    }

    const passwordHash = await hashPassword(newPassword);
    await db
      .update(userTable)
      .set({
        passwordHash,
        sessionVersion: sqlIncrement(userTable.sessionVersion),
        updatedAt: new Date(),
      })
      .where(eq(userTable.id, actor.id));

    return { success: true };
  }

  async deleteAccount(actor: AuthenticatedUser, password: string) {
    return this.withAdminMutationLock(() =>
      this.deleteAccountWithAdminMutationLock(actor, password),
    );
  }

  private async deleteAccountWithAdminMutationLock(
    actor: AuthenticatedUser,
    password: string,
  ) {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const user = await this.requireUser(actor.id);

    const passwordMatches = await bcrypt.compare(password, user.passwordHash);
    if (!passwordMatches) {
      throw new UnauthorizedException("Password is incorrect");
    }

    await this.assertAdminWillRemain(user, { isActive: false });

    await db
      .update(userTable)
      .set({
        isActive: false,
        sessionVersion: sqlIncrement(userTable.sessionVersion),
        updatedAt: new Date(),
      })
      .where(eq(userTable.id, actor.id));
    return { success: true };
  }

  private async requireUser(id: string): Promise<UserRecord> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const users = (await db
      .select()
      .from(userTable)
      .where(eq(userTable.id, id))) as UserRecord[];
    if (users.length === 0) {
      throw new NotFoundException("User not found");
    }
    return users[0];
  }

  private async assertAdminWillRemain(
    currentUser: UserRecord,
    input: { role?: UserRole; isActive?: boolean },
  ) {
    if (currentUser.role !== USER_ROLES.ADMIN || !currentUser.isActive) {
      return;
    }

    const wouldRemoveAdmin =
      input.role === USER_ROLES.USER || input.isActive === false;
    if (!wouldRemoveAdmin) {
      return;
    }

    const db = getDatabase();
    const userTable = this.getUserTable();
    const rows = (await db
      .select({ value: count() })
      .from(userTable)
      .where(
        and(eq(userTable.isActive, true), eq(userTable.role, USER_ROLES.ADMIN)),
      )) as { value: number }[];
    const activeAdminCount = Number(rows[0]?.value ?? 0);

    if (activeAdminCount <= 1) {
      throw new ForbiddenException("At least one active admin is required");
    }
  }
}

function sqlIncrement(column: AnyColumn) {
  return sql`${column} + 1`;
}

function wait(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
