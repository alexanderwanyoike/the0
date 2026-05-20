import {
  CanActivate,
  ExecutionContext,
  Injectable,
  UnauthorizedException,
} from "@nestjs/common";
import { AuthService } from "./auth.service";
import { AuthenticatedRequest } from "./auth.types";
import { extractAuthHeaderToken, toAuthenticatedJwtUser } from "./request-auth";

@Injectable()
export class JwtAuthGuard implements CanActivate {
  constructor(private readonly authService: AuthService) {}

  async canActivate(context: ExecutionContext): Promise<boolean> {
    const request = context.switchToHttp().getRequest<AuthenticatedRequest>();
    const token = extractAuthHeaderToken(request, "Bearer");

    if (!token) {
      throw new UnauthorizedException("Bearer token is required");
    }

    const result = await this.authService.validateToken(token);
    if (!result.success) {
      throw new UnauthorizedException(result.error);
    }

    request.user = toAuthenticatedJwtUser(result.data);

    return true;
  }
}
