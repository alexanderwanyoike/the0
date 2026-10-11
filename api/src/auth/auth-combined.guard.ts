import {
  Injectable,
  CanActivate,
  ExecutionContext,
  UnauthorizedException,
} from "@nestjs/common";
import { Reflector } from "@nestjs/core";
import { AuthService } from "./auth.service";
import { ApiKeyService } from "@/api-key/api-key.service";
import { AuthenticatedRequest } from "./auth.types";
import { ActorChannel } from "./actor";
import { ACTOR_VIA_KEY, ALLOW_ANONYMOUS_KEY } from "./actor.decorators";
import {
  extractApiKey,
  extractAuthHeaderToken,
  toAuthenticatedJwtUser,
} from "./request-auth";

@Injectable()
export class AuthCombinedGuard implements CanActivate {
  constructor(
    private readonly authService: AuthService,
    private readonly apiKeyService: ApiKeyService,
    private readonly reflector: Reflector,
  ) {}

  async canActivate(context: ExecutionContext): Promise<boolean> {
    try {
      return await this.authenticate(context);
    } catch (error) {
      if (
        error instanceof UnauthorizedException &&
        this.routeOption<boolean>(context, ALLOW_ANONYMOUS_KEY)
      ) {
        return true;
      }
      throw error;
    }
  }

  private async authenticate(context: ExecutionContext): Promise<boolean> {
    const request = context.switchToHttp().getRequest();
    const via = this.routeOption<ActorChannel>(context, ACTOR_VIA_KEY);

    const jwtToken = extractAuthHeaderToken(request, "Bearer");
    if (jwtToken) {
      return this.validateJwtToken(request, jwtToken, via ?? "jwt");
    }

    const apiKey = extractApiKey(request);
    if (apiKey) {
      return this.validateApiKey(request, apiKey, via ?? "api-key");
    }

    throw new UnauthorizedException(
      "Authentication required. Provide Bearer JWT token or ApiKey.",
    );
  }

  private routeOption<T>(
    context: ExecutionContext,
    key: string,
  ): T | undefined {
    return this.reflector.getAllAndOverride<T | undefined>(key, [
      context.getHandler(),
      context.getClass(),
    ]);
  }

  private async validateJwtToken(
    request: AuthenticatedRequest,
    token: string,
    via: ActorChannel,
  ): Promise<boolean> {
    const result = await this.authService.validateToken(token);

    if (!result.success) {
      throw new UnauthorizedException(result.error);
    }

    request.user = toAuthenticatedJwtUser(result.data);
    request.actor = { userId: result.data.id, via };

    return true;
  }

  private async validateApiKey(
    request: AuthenticatedRequest,
    apiKey: string,
    via: ActorChannel,
  ): Promise<boolean> {
    const result = await this.apiKeyService.validateApiKey(apiKey);

    if (!result.success) {
      throw new UnauthorizedException("Invalid API key");
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
    request.actor = {
      userId: result.data.userId,
      via,
      apiKeyId: result.data.id,
    };

    return true;
  }
}
