import {
  Injectable,
  CanActivate,
  ExecutionContext,
  UnauthorizedException,
} from "@nestjs/common";
import { AuthService } from "./auth.service";
import { ApiKeyService } from "@/api-key/api-key.service";
import { AuthenticatedRequest } from "./auth.types";
import { extractAuthHeaderToken, toAuthenticatedJwtUser } from "./request-auth";

@Injectable()
export class AuthCombinedGuard implements CanActivate {
  constructor(
    private readonly authService: AuthService,
    private readonly apiKeyService: ApiKeyService,
  ) {}

  async canActivate(context: ExecutionContext): Promise<boolean> {
    const request = context.switchToHttp().getRequest();

    const jwtToken = extractAuthHeaderToken(request, "Bearer");
    if (jwtToken) {
      return this.validateJwtToken(request, jwtToken);
    }

    const apiKey = extractAuthHeaderToken(request, "ApiKey");
    if (apiKey) {
      return this.validateApiKey(request, apiKey);
    }

    throw new UnauthorizedException(
      "Authentication required. Provide Bearer JWT token or ApiKey.",
    );
  }

  private async validateJwtToken(
    request: AuthenticatedRequest,
    token: string,
  ): Promise<boolean> {
    const result = await this.authService.validateToken(token);

    if (!result.success) {
      throw new UnauthorizedException(result.error);
    }

    request.user = toAuthenticatedJwtUser(result.data);

    return true;
  }

  private async validateApiKey(
    request: AuthenticatedRequest,
    apiKey: string,
  ): Promise<boolean> {
    const result = await this.apiKeyService.validateApiKey(apiKey);

    if (!result.success) {
      throw new UnauthorizedException(result.error);
    }

    request.user = {
      uid: result.data.userId,
      id: result.data.userId,
      username: "",
      email: "",
      firstName: null,
      lastName: null,
      isActive: true,
      isEmailVerified: true,
      role: "user",
      authType: "apikey",
    };

    return true;
  }
}
