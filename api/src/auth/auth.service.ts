import {
  Injectable,
  Logger,
  Scope,
  ServiceUnavailableException,
} from "@nestjs/common";
import { JwtService } from "@nestjs/jwt";
import { getDatabase } from "../database/connection";
import { usersTable, usersTableSqlite } from "../database/schema/users";
import { eq } from "drizzle-orm";
import * as bcrypt from "bcrypt";
import { Result } from "../common/result";
import { getDatabaseConfig } from "../database/connection";

const CONNECTION_ERROR_CODES = new Set([
  "ETIMEDOUT",
  "ENETUNREACH",
  "ECONNREFUSED",
  "ECONNRESET",
  "ENOTFOUND",
  "CONNECT_TIMEOUT",
]);

function isConnectionError(error: unknown): boolean {
  if (error instanceof AggregateError) {
    return error.errors.some(isConnectionError);
  }
  return CONNECTION_ERROR_CODES.has((error as any)?.code);
}

export interface AuthUser {
  id: string;
  username: string;
  email: string;
  firstName?: string;
  lastName?: string;
  isActive: boolean;
  isEmailVerified: boolean;
  role: "admin" | "user";
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
  adminEmail?: string;
  warning?: string;
  docsUrl: string;
}

@Injectable({ scope: Scope.DEFAULT })
export class AuthService {
  private readonly logger = new Logger(AuthService.name);

  constructor(private jwtService: JwtService) {}

  private getUserTable() {
    const config = getDatabaseConfig();
    return config.type === "sqlite" ? usersTableSqlite : usersTable;
  }

  private toAuthUser(user: any): AuthUser {
    return {
      id: user.id,
      username: user.username,
      email: user.email,
      firstName: user.firstName,
      lastName: user.lastName,
      isActive: Boolean(user.isActive),
      isEmailVerified: Boolean(user.isEmailVerified),
      role: user.role === "admin" ? "admin" : "user",
    };
  }

  private signUser(user: any): { token: string; user: AuthUser } {
    const authUser = this.toAuthUser(user);
    const token = this.jwtService.sign({
      sub: authUser.id,
      username: authUser.username,
      email: authUser.email,
      role: authUser.role,
    });

    return { token, user: authUser };
  }

  private getConfiguredAdminEmail(): string | undefined {
    const email = process.env.THE0_ADMIN_EMAIL?.trim();
    return email || undefined;
  }

  async getSetupStatus(): Promise<SetupStatus> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const users = await db.select().from(userTable);
    const activeUsers = users.filter((user: any) => Boolean(user.isActive));
    const hasAdmin = activeUsers.some((user: any) => user.role === "admin");
    const adminEmail = this.getConfiguredAdminEmail();

    return {
      setupRequired: users.length === 0,
      userCount: users.length,
      hasAdmin,
      adminConfigured: hasAdmin,
      adminEmailRequired: Boolean(adminEmail),
      adminEmail,
      warning:
        users.length > 0 && !hasAdmin
          ? "No admin configured. See docs/deployment/admin-bootstrap.md"
          : undefined,
      docsUrl: "/deployment/admin-bootstrap",
    };
  }

  async createFirstAdmin(
    credentials: SetupCredentials,
  ): Promise<Result<{ token: string; user: AuthUser }, string>> {
    try {
      const db = getDatabase();
      const userTable = this.getUserTable();
      const users = await db.select().from(userTable);

      if (users.length > 0) {
        return Failure("Setup is only available before users exist");
      }

      const configuredAdminEmail = this.getConfiguredAdminEmail();
      if (
        configuredAdminEmail &&
        credentials.email.trim() !== configuredAdminEmail
      ) {
        return Failure(`Setup must use ${configuredAdminEmail}`);
      }

      const passwordHash = await bcrypt.hash(credentials.password, 10);
      const newUsers = await db
        .insert(userTable)
        .values({
          username: credentials.username,
          email: credentials.email,
          passwordHash,
          firstName: credentials.firstName,
          lastName: credentials.lastName,
          role: "admin",
          isActive: true,
          isEmailVerified: true,
        })
        .returning();

      return {
        success: true,
        error: null,
        data: this.signUser(newUsers[0]),
      };
    } catch (error) {
      if (isConnectionError(error)) {
        throw new ServiceUnavailableException(
          "Database temporarily unavailable",
        );
      }
      return Failure("Setup failed");
    }
  }

  async bootstrapAdminFromExistingUsers(): Promise<void> {
    const db = getDatabase();
    const userTable = this.getUserTable();
    const users = await db.select().from(userTable);

    if (users.length === 0) {
      return;
    }

    for (const user of users) {
      const metadataRole = (user as any).metadata?.role;
      if (metadataRole === "admin" && (user as any).role !== "admin") {
        await db
          .update(userTable)
          .set({ role: "admin", updatedAt: new Date() })
          .where(eq(userTable.id, (user as any).id));
      }
    }

    const refreshedUsers = await db.select().from(userTable);
    const activeAdmins = refreshedUsers.filter(
      (user: any) => user.isActive && user.role === "admin",
    );
    if (activeAdmins.length > 0) {
      return;
    }

    const configuredAdminEmail = this.getConfiguredAdminEmail();
    if (configuredAdminEmail) {
      const matchingActiveUser = refreshedUsers.find(
        (user: any) =>
          user.isActive === true && user.email === configuredAdminEmail,
      );

      if (matchingActiveUser) {
        await db
          .update(userTable)
          .set({ role: "admin", updatedAt: new Date() })
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
      const payload = this.jwtService.verify(token);
      const db = getDatabase();
      const config = getDatabaseConfig();

      const userTable =
        config.type === "sqlite" ? usersTableSqlite : usersTable;
      const users = await db
        .select()
        .from(userTable)
        .where(eq(userTable.id, payload.sub));

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
      const users = await db
        .select()
        .from(userTable)
        .where(eq(userTable.email, credentials.email));

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

function Failure(error: string): Result<any, string> {
  return { success: false, error, data: null };
}
