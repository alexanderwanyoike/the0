import {
  Controller,
  Post,
  Param,
  Body,
  UseGuards,
  BadRequestException,
  NotFoundException,
  ServiceUnavailableException,
  GatewayTimeoutException,
} from "@nestjs/common";
import { ApiTags, ApiOperation, ApiResponse, ApiParam, ApiBody } from "@nestjs/swagger";
import { IsString, IsOptional, IsObject, IsNumber } from "class-validator";
import { BotQueryService, BotQueryErrorCode } from "./bot-query.service";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";

class ExecuteQueryDto {
  @IsString()
  query_path: string;

  @IsOptional()
  @IsObject()
  params?: Record<string, unknown>;

  @IsOptional()
  @IsNumber()
  timeout_sec?: number;
}

@ApiTags("Bot Query")
@Controller("query/:botId")
@UseGuards(AuthCombinedGuard)
export class BotQueryController {
  constructor(private readonly botQueryService: BotQueryService) {}

  /**
   * Execute a query against a bot.
   * POST /bots/:botId/query
   */
  @Post()
  @ApiOperation({ summary: "Execute a query against a bot" })
  @ApiParam({ name: "botId", description: "Bot identifier" })
  @ApiBody({
    description: "Query request",
    schema: {
      type: "object",
      required: ["query_path"],
      properties: {
        query_path: {
          type: "string",
          description: "Query path (e.g., /portfolio, /status)",
          example: "/portfolio",
        },
        params: {
          type: "object",
          description: "Query parameters",
          example: { symbol: "BTC/USD" },
        },
        timeout_sec: {
          type: "number",
          description: "Query timeout in seconds (default: 30)",
          example: 30,
        },
      },
    },
  })
  @ApiResponse({ status: 200, description: "Query executed successfully" })
  @ApiResponse({ status: 400, description: "Invalid query request" })
  @ApiResponse({ status: 404, description: "Bot not found" })
  @ApiResponse({ status: 503, description: "Runtime unavailable" })
  @ApiResponse({ status: 504, description: "Query timeout" })
  async executeQuery(
    @Param("botId") botId: string,
    @Body() body: ExecuteQueryDto,
  ) {
    if (!body.query_path) {
      throw new BadRequestException("query_path is required");
    }

    // Ensure query_path starts with /
    const queryPath = body.query_path.startsWith("/")
      ? body.query_path
      : `/${body.query_path}`;

    const result = await this.botQueryService.executeQuery(botId, {
      queryPath,
      params: body.params,
      timeoutSec: body.timeout_sec,
    });

    if (!result.success) {
      switch (result.error?.code) {
        case BotQueryErrorCode.BOT_NOT_FOUND:
          throw new NotFoundException("Bot not found or access denied");
        case BotQueryErrorCode.RUNTIME_UNAVAILABLE:
          throw new ServiceUnavailableException(result.error.message);
        case BotQueryErrorCode.TIMEOUT:
          throw new GatewayTimeoutException(result.error.message);
        default:
          throw new BadRequestException(result.error?.message || "Query failed");
      }
    }

    return {
      success: true,
      data: result.data,
      message: "Query executed successfully",
    };
  }
}
