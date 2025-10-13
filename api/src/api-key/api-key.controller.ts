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

@Controller("api-keys")
@UseGuards(AuthGuard())
export class ApiKeyController {
  constructor(private readonly apiKeyService: ApiKeyService) {}

  /**
   * Create a new API key
   * POST /api-keys
   */
  @Post()
  async createApiKey(
    @Request() req: any,
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

  /**
   * Get all API keys for the authenticated user
   * GET /api-keys
   */
  @Get()
  async getApiKeys(@Request() req: any): Promise<ApiKeyResponseDto[]> {
    console.log("🔑 API Key Controller GET /api-keys called");
    console.log(
      "🔑 Auth header:",
      req.headers.authorization ? "Present" : "Missing",
    );
    console.log("🔑 req.user:", JSON.stringify(req.user, null, 2));

    if (!req.user) {
      console.log("❌ No user found in request - authentication failed");
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

  /**
   * Get a specific API key by ID
   * GET /api-keys/:id
   */
  @Get(":id")
  async getApiKeyById(
    @Request() req: any,
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

  /**
   * Delete (deactivate) an API key
   * DELETE /api-keys/:id
   */
  @Delete(":id")
  async deleteApiKey(
    @Request() req: any,
    @Param("id") keyId: string,
  ): Promise<{ message: string }> {
    console.log("🔑 DELETE /api-keys/:id endpoint called with keyId:", keyId);
    console.log("🔑 User ID from request:", req.user.uid);

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

  /**
   * Get API key statistics
   * GET /api-keys/stats
   */
  @Get("stats/summary")
  async getApiKeyStats(
    @Request() req: any,
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
