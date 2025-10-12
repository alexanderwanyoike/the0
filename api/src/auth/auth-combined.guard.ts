import {
  Injectable,
  CanActivate,
  ExecutionContext,
  UnauthorizedException,
} from "@nestjs/common";
import { AuthService } from "./auth.service";
import { ApiKeyService } from "@/api-key/api-key.service";

@Injectable()
export class AuthCombinedGuard implements CanActivate {
  constructor(
    private readonly authService: AuthService,
    private readonly apiKeyService: ApiKeyService,
  ) {}

  async canActivate(context: ExecutionContext): Promise<boolean> {
    const request = context.switchToHttp().getRequest();

    // Check if request has JWT token
    const jwtToken = this.extractJwtFromHeader(request);
    if (jwtToken) {
      return this.validateJwtToken(request, jwtToken);
    }

    // Check if request has API key
    const apiKey = this.extractApiKeyFromHeader(request);
    if (apiKey) {
      return this.validateApiKey(request, apiKey);
    }

    throw new UnauthorizedException(
      "Authentication required. Provide Bearer JWT token or ApiKey.",
    );
  }

  private async validateJwtToken(
    request: any,
    token: string,
  ): Promise<boolean> {
    const result = await this.authService.validateToken(token);

    if (!result.success) {
      throw new UnauthorizedException(result.error);
    }

    // Attach user info to request for downstream use
    request.user = {
      uid: result.data.id,
      id: result.data.id,
      username: result.data.username,
      email: result.data.email,
      firstName: result.data.firstName,
      lastName: result.data.lastName,
      isActive: result.data.isActive,
      isEmailVerified: result.data.isEmailVerified,
      authType: "jwt",
    };

    return true;
  }

  private async validateApiKey(request: any, apiKey: string): Promise<boolean> {
    const result = await this.apiKeyService.validateApiKey(apiKey);

    if (!result.success) {
      throw new UnauthorizedException(result.error);
    }

    // Attach user info to request for downstream use
    request.user = {
      uid: result.data.userId,
      id: result.data.userId,
      username: "", // API keys don't have username
      email: "", // API keys don't have email
      firstName: null,
      lastName: null,
      isActive: true,
      isEmailVerified: true,
      authType: "apikey",
    };

    return true;
  }

  private extractJwtFromHeader(request: any): string | null {
    const authHeader = request.headers.authorization;

    if (authHeader) {
      const [type, token] = authHeader.split(" ");
      if (type === "Bearer" && token) {
        return token;
      }
    }

    return null;
  }

  private extractApiKeyFromHeader(request: any): string | null {
    const authHeader = request.headers.authorization;

    if (authHeader) {
      const [type, key] = authHeader.split(" ");
      if (type === "ApiKey" && key) {
        return key;
      }
    }

    return null;
  }
}
