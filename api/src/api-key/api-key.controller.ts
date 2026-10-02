import {
  Controller,
  Post,
  Get,
  Delete,
  Body,
  Param,
  HttpStatus,
  HttpException,
  UseGuards,
  Request,
} from "@nestjs/common";
import { ApiKeyService } from "@/api-key/api-key.service";
import { CreateApiKeyDto } from "@/api-key/dto/create-api-key.dto";
import { AuthGuard } from "@nestjs/passport";
import { ApiKeyCreatedResponseDto } from "@/api-key/dto/api-key-created-response.dto";
import { ApiKeyResponseDto } from "@/api-key/dto/api-key-response.dto"; // Assuming you have JWT auth
import { AuthenticatedRequest } from "@/auth/auth.types";
import {
  ApiNotFoundResponse,
  ApiOperation,
  ApiParam,
  ApiTags,
} from "@nestjs/swagger";
import { ApiJwtAuth } from "@/swagger/api-auth.decorators";

@ApiTags("api-keys")
@ApiJwtAuth()
@Controller("api-keys")
@UseGuards(AuthGuard())
export class ApiKeyController {
  constructor(private readonly apiKeyService: ApiKeyService) {}

  @Post()
  @ApiOperation({
    summary: "Create an API key",
    description: "The full key is only returned in this response.",
  })
  async createApiKey(
    @Request() req: AuthenticatedRequest,
    @Body() createApiKeyDto: CreateApiKeyDto,
  ): Promise<ApiKeyCreatedResponseDto> {
    const userId = req.user.uid;

    const result = await this.apiKeyService.createApiKey(
      userId,
      createApiKeyDto,
    );

    if (!result.success) {
      throw new HttpException(
        {
          statusCode: HttpStatus.BAD_REQUEST,
          message: result.error,
          error: "Bad Request",
        },
        HttpStatus.BAD_REQUEST,
      );
    }

    return result.data;
  }

  @Get()
  @ApiOperation({ summary: "List your API keys" })
  async getApiKeys(
    @Request() req: AuthenticatedRequest,
  ): Promise<ApiKeyResponseDto[]> {
    if (!req.user) {
      throw new Error("Authentication required");
    }

    const userId = req.user.uid;

    const result = await this.apiKeyService.getUserApiKeys(userId);

    if (!result.success) {
      throw new HttpException(
        {
          statusCode: HttpStatus.INTERNAL_SERVER_ERROR,
          message: result.error,
          error: "Internal Server Error",
        },
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }

    return result.data;
  }

  @Get(":id")
  @ApiOperation({ summary: "Get an API key" })
  @ApiParam({ name: "id", description: "API key ID" })
  @ApiNotFoundResponse({ description: "API key not found" })
  async getApiKeyById(
    @Request() req: AuthenticatedRequest,
    @Param("id") keyId: string,
  ): Promise<ApiKeyResponseDto> {
    const userId = req.user.uid;

    const result = await this.apiKeyService.getApiKeyById(userId, keyId);

    if (!result.success) {
      const status =
        result.error === "Not found"
          ? HttpStatus.NOT_FOUND
          : HttpStatus.INTERNAL_SERVER_ERROR;
      throw new HttpException(
        {
          statusCode: status,
          message: result.error,
          error:
            status === HttpStatus.NOT_FOUND
              ? "Not Found"
              : "Internal Server Error",
        },
        status,
      );
    }

    return result.data;
  }

  @Delete(":id")
  @ApiOperation({ summary: "Delete (deactivate) an API key" })
  @ApiParam({ name: "id", description: "API key ID" })
  @ApiNotFoundResponse({ description: "API key not found" })
  async deleteApiKey(
    @Request() req: AuthenticatedRequest,
    @Param("id") keyId: string,
  ): Promise<{ message: string }> {
    const userId = req.user.uid;

    const result = await this.apiKeyService.deleteApiKey(userId, keyId);

    if (!result.success) {
      const status =
        result.error === "API key not found"
          ? HttpStatus.NOT_FOUND
          : HttpStatus.INTERNAL_SERVER_ERROR;
      throw new HttpException(
        {
          statusCode: status,
          message: result.error,
          error:
            status === HttpStatus.NOT_FOUND
              ? "Not Found"
              : "Internal Server Error",
        },
        status,
      );
    }

    return { message: "API key deleted successfully" };
  }

  @Get("stats/summary")
  @ApiOperation({ summary: "Count your total and active API keys" })
  async getApiKeyStats(
    @Request() req: AuthenticatedRequest,
  ): Promise<{ total: number; active: number }> {
    const userId = req.user.uid;

    const result = await this.apiKeyService.getApiKeyStats(userId);

    if (!result.success) {
      throw new HttpException(
        {
          statusCode: HttpStatus.INTERNAL_SERVER_ERROR,
          message: result.error,
          error: "Internal Server Error",
        },
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }

    return result.data;
  }
}
