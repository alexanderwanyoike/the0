import { ExtractJwt, Strategy } from "passport-jwt";
import { PassportStrategy } from "@nestjs/passport";
import { Injectable, UnauthorizedException } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { PinoLogger } from "nestjs-pino";
import { USER_ROLES } from "@/user/user.constants";
import { UserRepository } from "@/user/user.repository";
import { UserRecord } from "@/user/user.types";

@Injectable()
export class JwtStrategy extends PassportStrategy(Strategy) {
  constructor(
    private readonly users: UserRepository,
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
      const user = (await this.users.findById(
        String(payload.sub),
      )) as UserRecord | null;

      if (!user) {
        throw new UnauthorizedException("User not found");
      }

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
