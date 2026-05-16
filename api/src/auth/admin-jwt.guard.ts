import {
  CanActivate,
  ExecutionContext,
  ForbiddenException,
  Injectable,
  UnauthorizedException,
} from "@nestjs/common";
import { Request } from "express";
import { AuthService } from "./auth.service";
import { AuthenticatedRequest } from "./auth.types";
import { USER_ROLES } from "@/user/user.constants";

@Injectable()
export class AdminJwtGuard implements CanActivate {
  constructor(private readonly authService: AuthService) {}

  async canActivate(context: ExecutionContext): Promise<boolean> {
    const request = context.switchToHttp().getRequest<AuthenticatedRequest>();
    const token = this.extractBearerToken(request);

    if (!token) {
      throw new UnauthorizedException("Admin JWT authentication required");
    }

    const result = await this.authService.validateToken(token);
    if (!result.success) {
      throw new UnauthorizedException(result.error);
    }

    if (result.data.role !== USER_ROLES.ADMIN) {
      throw new ForbiddenException("Admin role required");
    }

    request.user = {
      uid: result.data.id,
      id: result.data.id,
      username: result.data.username,
      email: result.data.email,
      firstName: result.data.firstName || null,
      lastName: result.data.lastName || null,
      isActive: result.data.isActive,
      isEmailVerified: result.data.isEmailVerified,
      role: result.data.role,
      authType: "jwt",
    };

    return true;
  }

  private extractBearerToken(request: Request): string | null {
    const authHeader = request.headers.authorization;
    if (!authHeader) {
      return null;
    }

    const [type, token] = authHeader.split(" ");
    return type === "Bearer" && token ? token : null;
  }
}
