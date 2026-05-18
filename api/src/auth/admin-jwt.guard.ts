import {
  CanActivate,
  ExecutionContext,
  ForbiddenException,
  Injectable,
  UnauthorizedException,
} from "@nestjs/common";
import { AuthService } from "./auth.service";
import { AuthenticatedRequest } from "./auth.types";
import { USER_ROLES } from "@/user/user.constants";
import { extractAuthHeaderToken, toAuthenticatedJwtUser } from "./request-auth";

@Injectable()
export class AdminJwtGuard implements CanActivate {
  constructor(private readonly authService: AuthService) {}

  async canActivate(context: ExecutionContext): Promise<boolean> {
    const request = context.switchToHttp().getRequest<AuthenticatedRequest>();
    const token = extractAuthHeaderToken(request, "Bearer");

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

    request.user = toAuthenticatedJwtUser(result.data);

    return true;
  }
}
