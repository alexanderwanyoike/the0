import { ExtractJwt, Strategy } from "passport-jwt";
import { PassportStrategy } from "@nestjs/passport";
import { Injectable, UnauthorizedException } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { PinoLogger } from "nestjs-pino";
import { AuthService } from "./auth.service";
import { getDatabase, getDatabaseConfig } from "../database/connection";
import { usersTable, usersTableSqlite } from "../database/schema/users";
import { eq } from "drizzle-orm";
import { USER_ROLES } from "@/user/user.constants";
import { UserRecord } from "@/user/user.types";

@Injectable()
export class JwtStrategy extends PassportStrategy(Strategy) {
  constructor(
    private authService: AuthService,
    private readonly logger: PinoLogger,
    configService: ConfigService,
  ) {
    super({
      jwtFromRequest: ExtractJwt.fromAuthHeaderAsBearerToken(),
      ignoreExpiration: false,
      secretOrKey: configService.getOrThrow<string>("JWT_SECRET"),
      issuer: "the0-oss-api",
      audience: "the0-oss-clients",
    });
  }

  async validate(payload: Record<string, unknown>) {
    if (!payload?.sub) {
      throw new UnauthorizedException("Invalid token payload");
    }

    try {
      const db = getDatabase();
      const config = getDatabaseConfig();
      const userTable =
        config.type === "sqlite" ? usersTableSqlite : usersTable;

      const users = (await db
        .select()
        .from(userTable)
        .where(eq(userTable.id, String(payload.sub)))) as UserRecord[];

      if (!users || users.length === 0) {
        throw new UnauthorizedException("User not found");
      }

      const user = users[0];
      if (!user?.isActive) {
        throw new UnauthorizedException("User inactive");
      }

      if (
        (Number(payload.sessionVersion) || 0) !== (user.sessionVersion ?? 0)
      ) {
        throw new UnauthorizedException("Token has been invalidated");
      }

      return {
        uid: user.id || "",
        id: user.id || "",
        username: user.username || "",
        email: user.email || "",
        firstName: user.firstName || null,
        lastName: user.lastName || null,
        isActive: Boolean(user.isActive),
        isEmailVerified: Boolean(user.isEmailVerified),
        role:
          user.role === USER_ROLES.ADMIN ? USER_ROLES.ADMIN : USER_ROLES.USER,
        authType: "jwt" as const,
      };
    } catch (error) {
      if (error instanceof UnauthorizedException) {
        throw error;
      }
      this.logger.error({ err: error }, "JWT validation error");
      throw new UnauthorizedException("Token validation failed");
    }
  }
}
