import {
  Injectable,
  Logger,
  Scope,
  ServiceUnavailableException,
} from "@nestjs/common";
import { JwtService } from "@nestjs/jwt";
import { getDatabase } from "../database/connection";
import {
  type SetupLock,
  setupLocksTable,
  setupLocksTableSqlite,
  usersTable,
  usersTableSqlite,
} from "../database/schema/users";
import { and, count, eq } from "drizzle-orm";
import * as bcrypt from "bcrypt";
import { Failure, Result } from "../common/result";
import { getDatabaseConfig } from "../database/connection";
import { hashPassword } from "@/common/password";
import { USER_ROLES, UserRole } from "@/user/user.constants";
import { UserRecord } from "@/user/user.types";

const CONNECTION_ERROR_CODES = new Set([
  "ETIMEDOUT",
  "ENETUNREACH",
  "ECONNREFUSED",
  "ECONNRESET",
  "ENOTFOUND",
  "CONNECT_TIMEOUT",
]);

const SETUP_LOCK_ID = "first-admin";
const SETUP_LOCK_STALE_MS = 5 * 60 * 1000;

function isConnectionError(error: unknown): boolean {
  if (error instanceof AggregateError) {
    return error.errors.some(isConnectionError);
  }
  const code = (error as { code?: unknown })?.code;
  return typeof code === "string" && CONNECTION_ERROR_CODES.has(code);
}

export interface AuthUser {
  id: string;
  username: string;
  email: string;
  firstName?: string;
  lastName?: string;
  isActive: boolean;
  isEmailVerified: boolean;
  role: UserRole;
}

export interface LoginCredentials {
  email: string;
  password: string;
}

export interface SetupCredentials {
  username: string;
  email: string;
  password: string;
  firstName?: string;
  lastName?: string;
}

export interface SetupStatus {
  setupRequired: boolean;
  userCount: number;
  hasAdmin: boolean;
  adminConfigured: boolean;
  adminEmailRequired: boolean;
  warning?: string;
  docsUrl: string;
}

interface JwtPayload {
  sub: string;
  username?: string;
  email?: string;
  role?: UserRole;
  sessionVersion?: number;
}

function metadataRole(metadata: unknown): string | undefined {
  if (!metadata || typeof metadata !== "object") {
    return undefined;
  }
  const role = (metadata as { role?: unknown }).role;
  return typeof role === "string" ? role : undefined;
}

@Injectable({ scope: Scope.DEFAULT })
export class AuthService {
  private readonly logger = new Logger(AuthService.name);
  private setupLock = Promise.resolve();

  constructor(private jwtService: JwtService) {}

  private getUserTable() {
    const config = getDatabaseConfig();
    return config.type === "sqlite" ? usersTableSqlite : usersTable;
  }

  private getSetupLockTable() {
    const config = getDatabaseConfig();
    return config.type === "sqlite" ? setupLocksTableSqlite : setupLocksTable;
  }

  private toAuthUser(user: UserRecord): AuthUser {
    return {
      id: user.id,
      username: user.username,
      email: user.email,
      firstName: user.firstName ?? undefined,
      lastName: user.lastName ?? undefined,
      isActive: Boolean(user.isActive),
      isEmailVerified: Boolean(user.isEmailVerified),
      role: user.role === USER_ROLES.ADMIN ? USER_ROLES.ADMIN : USER_ROLES.USER,
    };
  }

  private signUser(user: UserRecord): { token: string; user: AuthUser } {
    const authUser = this.toAuthUser(user);
    const token = this.jwtService.sign({
      sub: authUser.id,
      username: authUser.username,
      email: authUser.email,
      role: authUser.role,
      sessionVersion: user.sessionVersion,
    });

    return { token, user: authUser };
  }

  private async withSetupLock<T>(callback: () => Promise<T>): Promise<T> {
    const previousLock = this.setupLock;
    let releaseLock: () => void = () => undefined;
    this.setupLock = new Promise<void>((resolve) => {
      releaseLock = resolve;
    });

    await previousLock;
    try {
      return await callback();
    } finally {
      releaseLock();
    }
  }

  private getConfiguredAdminEmail(): string | undefined {
    const email = process.env.THE0_ADMIN_EMAIL?.trim();
    return email || undefined;
  }

  private async getUserCount(): Promise<number> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const rows = (await db.select({ value: count() }).from(userTable)) as {
      value: number;
    }[];
    return Number(rows[0]?.value ?? 0);
  }

  private async hasActiveAdmin(): Promise<boolean> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const rows = (await db
      .select({ id: userTable.id })
      .from(userTable)
      .where(
        and(eq(userTable.isActive, true), eq(userTable.role, USER_ROLES.ADMIN)),
      )
      .limit(1)) as { id: string }[];
    return rows.length > 0;
  }

  async getSetupStatus(): Promise<SetupStatus> {
    const userCount = await this.getUserCount();
    const hasAdmin = await this.hasActiveAdmin();
    const adminEmail = this.getConfiguredAdminEmail();

    return {
      setupRequired: userCount === 0,
      userCount,
      hasAdmin,
      adminConfigured: hasAdmin,
      adminEmailRequired: Boolean(adminEmail),
      warning:
        userCount > 0 && !hasAdmin
          ? "No admin configured. See docs/deployment/admin-bootstrap.md"
          : undefined,
      docsUrl: "/deployment/admin-bootstrap",
    };
  }

  async createFirstAdmin(
    credentials: SetupCredentials,
  ): Promise<Result<{ token: string; user: AuthUser }, string>> {
    return this.withSetupLock(async () => {
      let setupLockClaimed = false;
      try {
        const db = getDatabase();
        const userTable = this.getUserTable();
        const userCount = await this.getUserCount();

        if (userCount > 0) {
          return Failure("Setup is only available before users exist");
        }

        const configuredAdminEmail = this.getConfiguredAdminEmail();
        if (
          configuredAdminEmail &&
          credentials.email.trim() !== configuredAdminEmail
        ) {
          return Failure("Setup email does not match configured admin email");
        }

        const passwordHash = await hashPassword(credentials.password);
        setupLockClaimed = await this.acquireSetupLock();
        if (!setupLockClaimed) {
          return Failure("Setup is already in progress");
        }

        const usersBeforeInsert = await this.getUserCount();
        if (usersBeforeInsert > 0) {
          await this.releaseSetupLock().catch((): void => undefined);
          return Failure("Setup is only available before users exist");
        }

        const newUsers = (await db
          .insert(userTable)
          .values({
            username: credentials.username.trim(),
            email: credentials.email.trim(),
            passwordHash,
            firstName: credentials.firstName,
            lastName: credentials.lastName,
            role: USER_ROLES.ADMIN,
            isActive: true,
            isEmailVerified: true,
          })
          .returning()) as UserRecord[];

        return {
          success: true,
          error: null,
          data: this.signUser(newUsers[0]),
        };
      } catch (error) {
        if (isConnectionError(error)) {
          if (setupLockClaimed) {
            await this.releaseSetupLock().catch((): void => undefined);
          }
          throw new ServiceUnavailableException(
            "Database temporarily unavailable",
          );
        }
        if (setupLockClaimed) {
          await this.releaseSetupLock().catch((): void => undefined);
        }
        return Failure("Setup failed");
      }
    });
  }

  private async acquireSetupLock(): Promise<boolean> {
    const db = getDatabase();
    const setupLockTable = this.getSetupLockTable();
    try {
      await db
        .insert(setupLockTable)
        .values({ id: SETUP_LOCK_ID, lockedAt: Date.now() });
      return true;
    } catch (error) {
      if (isConnectionError(error)) {
        throw error;
      }

      const locks = (await db
        .select()
        .from(setupLockTable)
        .where(eq(setupLockTable.id, SETUP_LOCK_ID))) as SetupLock[];
      const existingLock = locks[0];
      if (
        existingLock &&
        Date.now() - Number(existingLock.lockedAt) > SETUP_LOCK_STALE_MS
      ) {
        await this.releaseSetupLock();
        return this.acquireSetupLock();
      }

      return false;
    }
  }

  private async releaseSetupLock(): Promise<void> {
    const db = getDatabase();
    const setupLockTable = this.getSetupLockTable();
    await db.delete(setupLockTable).where(eq(setupLockTable.id, SETUP_LOCK_ID));
  }

  async bootstrapAdminFromExistingUsers(): Promise<void> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const users = (await db.select().from(userTable)) as UserRecord[];

    if (users.length === 0) {
      return;
    }

    for (const user of users) {
      const roleFromMetadata = metadataRole(user.metadata);
      if (
        roleFromMetadata === USER_ROLES.ADMIN &&
        user.role !== USER_ROLES.ADMIN
      ) {
        await db
          .update(userTable)
          .set({ role: USER_ROLES.ADMIN, updatedAt: new Date() })
          .where(eq(userTable.id, user.id));
      }
    }

    const refreshedUsers = (await db.select().from(userTable)) as UserRecord[];
    const activeAdmins = refreshedUsers.filter(
      (user) => user.isActive && user.role === USER_ROLES.ADMIN,
    );
    if (activeAdmins.length > 0) {
      return;
    }

    const configuredAdminEmail = this.getConfiguredAdminEmail();
    if (configuredAdminEmail) {
      const matchingActiveUser = refreshedUsers.find(
        (user) => user.isActive === true && user.email === configuredAdminEmail,
      );

      if (matchingActiveUser) {
        await db
          .update(userTable)
          .set({ role: USER_ROLES.ADMIN, updatedAt: new Date() })
          .where(eq(userTable.id, matchingActiveUser.id));
        this.logger.log(
          `Promoted configured admin user ${configuredAdminEmail}`,
        );
        return;
      }
    }

    this.logger.warn(
      "No admin configured. Set THE0_ADMIN_EMAIL to an existing active user or see docs/deployment/admin-bootstrap.md",
    );
  }

  async validateToken(token: string): Promise<Result<AuthUser, string>> {
    try {
      const payload = this.jwtService.verify<JwtPayload>(token);
      const db = getDatabase();
      const config = getDatabaseConfig();

      const userTable =
        config.type === "sqlite" ? usersTableSqlite : usersTable;
      const users = (await db
        .select()
        .from(userTable)
        .where(eq(userTable.id, payload.sub))) as UserRecord[];

      if (users.length === 0) {
        return {
          success: false,
          error: "User not found",
          data: null,
        };
      }

      const user = users[0];
      if (!user.isActive) {
        return {
          success: false,
          error: "User account is inactive",
          data: null,
        };
      }

      if ((payload.sessionVersion ?? 0) !== (user.sessionVersion ?? 0)) {
        return {
          success: false,
          error: "Invalid token",
          data: null,
        };
      }

      return {
        success: true,
        error: null,
        data: {
          ...this.toAuthUser(user),
        },
      };
    } catch (error) {
      if (isConnectionError(error)) {
        throw new ServiceUnavailableException(
          "Database temporarily unavailable",
        );
      }
      return {
        success: false,
        error: "Invalid token",
        data: null,
      };
    }
  }

  async login(
    credentials: LoginCredentials,
  ): Promise<Result<{ token: string; user: AuthUser }, string>> {
    try {
      const db = getDatabase();
      const config = getDatabaseConfig();

      const userTable =
        config.type === "sqlite" ? usersTableSqlite : usersTable;
      const users = (await db
        .select()
        .from(userTable)
        .where(eq(userTable.email, credentials.email))) as UserRecord[];

      if (users.length === 0) {
        return {
          success: false,
          error: "Invalid credentials",
          data: null,
        };
      }

      const user = users[0];
      if (!user.isActive) {
        return {
          success: false,
          error: "User account is inactive",
          data: null,
        };
      }

      const isPasswordValid = await bcrypt.compare(
        credentials.password,
        user.passwordHash,
      );
      if (!isPasswordValid) {
        return {
          success: false,
          error: "Invalid credentials",
          data: null,
        };
      }

      // Update last login
      await db
        .update(userTable)
        .set({ lastLoginAt: new Date() })
        .where(eq(userTable.id, user.id));

      return {
        success: true,
        error: null,
        data: this.signUser(user),
      };
    } catch (error) {
      if (isConnectionError(error)) {
        throw new ServiceUnavailableException(
          "Database temporarily unavailable",
        );
      }
      return {
        success: false,
        error: "Login failed",
        data: null,
      };
    }
  }
}
