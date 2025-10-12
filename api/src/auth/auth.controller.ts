import {
  Body,
  Controller,
  Post,
  Get,
  Headers,
  UnauthorizedException,
} from "@nestjs/common";
import {
  AuthService,
  LoginCredentials,
  RegisterCredentials,
} from "./auth.service";
import { ValidateTokenDto } from "./dto/validate-token.dto";
import { ApiTags, ApiBearerAuth } from "@nestjs/swagger";
import { IsEmail, IsString, IsNotEmpty, IsOptional } from "class-validator";
import { ApiKeyService } from "@/api-key/api-key.service";

export class LoginDto {
  @IsEmail()
  @IsNotEmpty()
  email: string;

  @IsString()
  @IsNotEmpty()
  password: string;
}

export class RegisterDto {
  @IsString()
  @IsNotEmpty()
  username: string;

  @IsEmail()
  @IsNotEmpty()
  email: string;

  @IsString()
  @IsNotEmpty()
  password: string;

  @IsString()
  @IsOptional()
  firstName?: string;

  @IsString()
  @IsOptional()
  lastName?: string;
}

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

  @Post("register")
  async register(@Body() registerDto: RegisterDto) {
    const result = await this.authService.register(registerDto);

    if (!result.success) {
      throw new UnauthorizedException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Registration successful",
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
