import {
  Controller,
  Post,
  Body,
  Headers,
  HttpCode,
  HttpStatus,
  UnauthorizedException,
} from "@nestjs/common";
import { ApiTags, ApiOperation, ApiHeader, ApiBody } from "@nestjs/swagger";
import { McpService } from "./mcp.service";
import { MCP_TOOL_DEFINITIONS } from "./mcp.types";
import { ApiKeyService } from "@/api-key/api-key.service";
import { PinoLogger } from "nestjs-pino";

/**
 * JSON-RPC 2.0 Request structure
 */
interface JsonRpcRequest {
  jsonrpc: "2.0";
  id: string | number;
  method: string;
  params?: Record<string, unknown>;
}

/**
 * JSON-RPC 2.0 Response structure
 */
interface JsonRpcResponse {
  jsonrpc: "2.0";
  id: string | number | null;
  result?: unknown;
  error?: {
    code: number;
    message: string;
    data?: unknown;
  };
}

@ApiTags("MCP")
@Controller("mcp")
export class McpController {
  constructor(
    private readonly mcpService: McpService,
    private readonly apiKeyService: ApiKeyService,
    private readonly logger: PinoLogger,
  ) {}

  @Post()
  @HttpCode(HttpStatus.OK)
  @ApiOperation({
    summary: "MCP JSON-RPC endpoint",
    description:
      "Handle MCP JSON-RPC 2.0 requests for tool discovery and execution",
  })
  @ApiHeader({
    name: "x-api-key",
    description: "API key for authentication",
    required: true,
  })
  @ApiBody({
    description: "JSON-RPC 2.0 request",
    schema: {
      type: "object",
      properties: {
        jsonrpc: { type: "string", example: "2.0" },
        id: { type: "string", example: "1" },
        method: { type: "string", example: "tools/list" },
        params: { type: "object" },
      },
    },
  })
  async handleRpc(
    @Body() request: JsonRpcRequest,
    @Headers("x-api-key") apiKey?: string,
  ): Promise<JsonRpcResponse> {
    this.logger.info({ method: request.method }, "MCP request received");

    // Validate API key
    let userId: string | undefined;
    if (apiKey) {
      const validation = await this.apiKeyService.validateApiKey(apiKey);
      if (validation.success && validation.data) {
        userId = validation.data.userId;
      }
    }

    try {
      // Handle different MCP methods
      switch (request.method) {
        case "initialize":
          return this.createResponse(request.id, {
            protocolVersion: "2024-11-05",
            capabilities: {
              tools: {},
            },
            serverInfo: {
              name: "the0-mcp",
              version: "1.0.0",
            },
          });

        case "tools/list":
          return this.createResponse(request.id, {
            tools: MCP_TOOL_DEFINITIONS,
          });

        case "tools/call":
          if (!apiKey || !userId) {
            return this.createErrorResponse(
              request.id,
              -32001,
              "Authentication required. Provide x-api-key header.",
            );
          }
          const toolName = request.params?.name as string;
          const toolArgs = (request.params?.arguments || {}) as Record<
            string,
            unknown
          >;

          if (!toolName) {
            return this.createErrorResponse(
              request.id,
              -32602,
              "Missing tool name",
            );
          }

          const result = await this.mcpService.handleToolCall(
            toolName,
            toolArgs,
            userId,
          );
          return this.createResponse(request.id, result);

        case "ping":
          return this.createResponse(request.id, {});

        default:
          return this.createErrorResponse(
            request.id,
            -32601,
            `Method not found: ${request.method}`,
          );
      }
    } catch (error) {
      this.logger.error(
        { error, method: request.method },
        "MCP request failed",
      );
      return this.createErrorResponse(
        request.id,
        -32603,
        error instanceof Error ? error.message : "Internal error",
      );
    }
  }

  private createResponse(
    id: string | number | null,
    result: unknown,
  ): JsonRpcResponse {
    return {
      jsonrpc: "2.0",
      id,
      result,
    };
  }

  private createErrorResponse(
    id: string | number | null,
    code: number,
    message: string,
    data?: unknown,
  ): JsonRpcResponse {
    return {
      jsonrpc: "2.0",
      id,
      error: {
        code,
        message,
        data,
      },
    };
  }
}
