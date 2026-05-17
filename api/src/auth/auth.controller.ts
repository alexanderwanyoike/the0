import {
  Body,
  Controller,
  Post,
  Get,
  Headers,
  UnauthorizedException,
  BadRequestException,
} from "@nestjs/common";
import { AuthService } from "./auth.service";
import { LoginDto } from "./dto/login.dto";
import { SetupDto } from "./dto/setup.dto";
import { ValidateTokenDto } from "./dto/validate-token.dto";
import { ApiTags, ApiBearerAuth } from "@nestjs/swagger";
import { ApiKeyService } from "@/api-key/api-key.service";

@ApiTags("auth")
@Controller("auth")
export class AuthController {
  constructor(
    private readonly authService: AuthService,
    private readonly apiKeyService: ApiKeyService,
  ) {}

  @Post("login")
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

  @Get("setup-status")
  async setupStatus() {
    const status = await this.authService.getSetupStatus();

    return {
      success: true,
      data: status,
      message: "Setup status retrieved successfully",
    };
  }

  @Post("setup")
  async setup(@Body() setupDto: SetupDto) {
    const result = await this.authService.createFirstAdmin(setupDto);

    if (!result.success) {
      throw new BadRequestException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Setup completed successfully",
    };
  }

  @Post("validate")
  @ApiBearerAuth()
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
  @ApiBearerAuth()
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
      throw new UnauthorizedException(result.error);
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
