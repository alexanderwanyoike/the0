import { Controller, Post, Param, Body, UseGuards } from "@nestjs/common";
import {
  ApiTags,
  ApiOperation,
  ApiResponse,
  ApiParam,
  ApiBody,
} from "@nestjs/swagger";
import { BotQueryService } from "./bot-query.service";
import { ExecuteQueryDto } from "./dto/execute-query.dto";
import { Actor } from "@/auth/actor";
import { CurrentActor } from "@/auth/actor.decorators";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";
import { toHttpException } from "@/common/http-error";
import { ApiJwtOrApiKeyAuth } from "@/swagger/api-auth.decorators";

@ApiTags("bot-query")
@ApiJwtOrApiKeyAuth()
@Controller("query/:botId")
@UseGuards(AuthCombinedGuard)
export class BotQueryController {
  constructor(private readonly botQueryService: BotQueryService) {}

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
  @ApiResponse({
    status: 422,
    description: "Bot defines no queries (no query entrypoint)",
  })
  @ApiResponse({ status: 503, description: "Runtime unavailable" })
  @ApiResponse({ status: 504, description: "Query timeout" })
  async executeQuery(
    @CurrentActor() actor: Actor,
    @Param("botId") botId: string,
    @Body() body: ExecuteQueryDto,
  ) {
    // Ensure query_path starts with /
    const queryPath = body.query_path.startsWith("/")
      ? body.query_path
      : `/${body.query_path}`;

    const result = await this.botQueryService.executeQuery(actor, botId, {
      queryPath,
      params: body.params,
      timeoutSec: body.timeout_sec,
    });

    if (!result.success) {
      throw toHttpException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Query executed successfully",
    };
  }
}
