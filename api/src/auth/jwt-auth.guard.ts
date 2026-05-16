import {
  CanActivate,
  ExecutionContext,
  Injectable,
  UnauthorizedException,
} from "@nestjs/common";
import { Request } from "express";
import { AuthService } from "./auth.service";
import { AuthenticatedRequest } from "./auth.types";

@Injectable()
export class JwtAuthGuard implements CanActivate {
  constructor(private readonly authService: AuthService) {}

  async canActivate(context: ExecutionContext): Promise<boolean> {
    const request = context.switchToHttp().getRequest<AuthenticatedRequest>();
    const token = this.extractBearerToken(request);

    if (!token) {
      throw new UnauthorizedException("Bearer token is required");
    }

    const result = await this.authService.validateToken(token);
    if (!result.success) {
      throw new UnauthorizedException(result.error);
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
