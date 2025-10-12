import {
  Injectable,
  CanActivate,
  ExecutionContext,
  UnauthorizedException,
} from "@nestjs/common";
import { ApiKeyService } from "@/api-key/api-key.service";

@Injectable()
export class ApiKeyGuard implements CanActivate {
  constructor(private readonly apiKeyService: ApiKeyService) {}

  async canActivate(context: ExecutionContext): Promise<boolean> {
    const request = context.switchToHttp().getRequest();
    const apiKey = this.extractApiKeyFromHeader(request);

    if (!apiKey) {
      throw new UnauthorizedException("API key is required");
    }

    const result = await this.apiKeyService.validateApiKey(apiKey);

    if (!result.success) {
      throw new UnauthorizedException("Invalid API key");
    }

    // Attach the user to the request for downstream use
    request.user = {
      uid: result.data.userId,
      apiKeyId: result.data.id,
      apiKeyName: result.data.name,
    };

    return true;
  }

  private extractApiKeyFromHeader(request: any): string | null {
    // Support multiple header formats:
    // Authorization: Bearer the0_xxx
    // Authorization: ApiKey the0_xxx
    // X-API-Key: the0_xxx

    const authHeader = request.headers.authorization;
    const apiKeyHeader = request.headers["x-api-key"];

    if (apiKeyHeader) {
      return apiKeyHeader;
    }

    if (authHeader) {
      const [type, key] = authHeader.split(" ");
      if ((type === "Bearer" || type === "ApiKey") && key) {
        return key;
      }
    }

    return null;
  }
}
