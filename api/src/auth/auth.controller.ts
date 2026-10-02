import {
  Body,
  Controller,
  Get,
  Post,
  Headers,
  UnauthorizedException,
} from "@nestjs/common";
import { AuthService } from "./auth.service";
import { LoginDto } from "./dto/login.dto";
import { ValidateTokenDto } from "./dto/validate-token.dto";
import {
  ApiHeader,
  ApiOperation,
  ApiTags,
  ApiUnauthorizedResponse,
} from "@nestjs/swagger";
import { ApiKeyService } from "@/api-key/api-key.service";
import { ApiJwtAuth, ApiKeySecurity } from "@/swagger/api-auth.decorators";

// The header is read by hand here, so Swagger would list it as a required
// parameter; the security scheme already sends it.
const AUTHORIZATION_HEADER = {
  name: "authorization",
  required: false,
  description: "Filled in by Authorize",
};

@ApiTags("auth")
@Controller("auth")
export class AuthController {
  constructor(
    private readonly authService: AuthService,
    private readonly apiKeyService: ApiKeyService,
  ) {}

  @Post("login")
  @ApiOperation({ summary: "Log in with email and password to get a JWT" })
  @ApiUnauthorizedResponse({ description: "Invalid credentials" })
  async login(@Body() loginDto: LoginDto) {
    const result = await this.authService.login(loginDto);

    if (!result.success) {
      throw new UnauthorizedException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Login successful",
    };
  }

  @Post("validate")
  @ApiOperation({ summary: "Check whether a JWT passed in the body is valid" })
  @ApiUnauthorizedResponse({ description: "Token is invalid or expired" })
  async validate(@Body() validateTokenDto: ValidateTokenDto) {
    const result = await this.authService.validateToken(validateTokenDto.token);

    if (!result.success) {
      throw new UnauthorizedException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Token is valid",
    };
  }

  @Get("me")
  @ApiOperation({ summary: "Get the user the JWT belongs to" })
  @ApiJwtAuth()
  @ApiHeader(AUTHORIZATION_HEADER)
  async getCurrentUser(@Headers("authorization") authHeader?: string) {
    if (!authHeader || !authHeader.startsWith("Bearer ")) {
      throw new UnauthorizedException("Bearer token is required");
    }

    const token = authHeader.substring(7);
    const result = await this.authService.validateToken(token);

    if (!result.success) {
      throw new UnauthorizedException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "User retrieved successfully",
    };
  }

  @Get("validate-api-key")
  @ApiOperation({
    summary: "Check whether an API key is valid",
    description: "Also accepts the key as `Authorization: Bearer <key>`.",
  })
  @ApiKeySecurity()
  @ApiHeader(AUTHORIZATION_HEADER)
  async validateApiKey(@Headers("authorization") authHeader?: string) {
    if (!authHeader) {
      throw new UnauthorizedException("Authorization header is required");
    }

    // Extract API key from Authorization header
    let apiKey: string;
    if (authHeader.startsWith("ApiKey ")) {
      apiKey = authHeader.substring(7);
    } else if (authHeader.startsWith("Bearer ")) {
      apiKey = authHeader.substring(7);
    } else {
      throw new UnauthorizedException("Invalid authorization header format");
    }

    const result = await this.apiKeyService.validateApiKey(apiKey);

    if (!result.success) {
      throw new UnauthorizedException("Invalid API key");
    }

    return {
      success: true,
      data: {
        valid: true,
        userId: result.data.userId,
        keyId: result.data.id,
        keyName: result.data.name,
        lastUsedAt: result.data.lastUsedAt
          ? result.data.lastUsedAt.toISOString()
          : null,
      },
      message: "API key is valid",
    };
  }
}
