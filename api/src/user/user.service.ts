import {
  BadRequestException,
  ForbiddenException,
  Injectable,
  NotFoundException,
  UnauthorizedException,
} from "@nestjs/common";
import { and, eq, ne, sql, type AnyColumn } from "drizzle-orm";
import * as bcrypt from "bcrypt";
import { hashPassword } from "@/common/password";
import { getDatabase, getDatabaseConfig } from "@/database/connection";
import { usersTable, usersTableSqlite } from "@/database/schema/users";
import { AuthenticatedUser } from "@/auth/auth.types";
import { USER_ROLES, UserRole } from "./user.constants";
import {
  CreateAdminUserInput,
  SerializedUser,
  UpdateAdminUserInput,
  UpdateProfileInput,
  UserRecord,
} from "./user.types";

@Injectable()
export class UserService {
  private adminMutationLock = Promise.resolve();

  private getUserTable() {
    const config = getDatabaseConfig();
    return config.type === "sqlite" ? usersTableSqlite : usersTable;
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
      return await callback();
    } finally {
      releaseLock();
    }
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

    await db.delete(userTable).where(eq(userTable.id, actor.id));
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
    const users = (await db.select().from(userTable)) as UserRecord[];
    const activeAdminCount = users.filter(
      (user) => user.isActive && user.role === USER_ROLES.ADMIN,
    ).length;

    if (activeAdminCount <= 1) {
      throw new ForbiddenException("At least one active admin is required");
    }
  }
}

function sqlIncrement(column: AnyColumn) {
  return sql`${column} + 1`;
}
