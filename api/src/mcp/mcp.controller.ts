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
import { ApiKeyService } from "@/api-key/api-key.service";
import { InjectPinoLogger, PinoLogger } from "nestjs-pino";

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
    @InjectPinoLogger(McpController.name)
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
            tools: this.getToolDefinitions(),
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
      this.logger.error({ error, method: request.method }, "MCP request failed");
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

  private getToolDefinitions() {
    return [
      // Auth Tools
      {
        name: "auth_status",
        description: "Check if the API key is valid and get connection status",
        inputSchema: {
          type: "object",
          properties: {},
          required: [],
        },
      },

      // Bot Instance Tools
      {
        name: "bot_list",
        description:
          "List all deployed bot instances for the authenticated user",
        inputSchema: {
          type: "object",
          properties: {},
          required: [],
        },
      },
      {
        name: "bot_get",
        description: "Get details of a specific bot instance",
        inputSchema: {
          type: "object",
          properties: {
            bot_id: {
              type: "string",
              description: "The bot instance ID",
            },
          },
          required: ["bot_id"],
        },
      },
      {
        name: "bot_deploy",
        description: "Deploy a new bot instance with the given configuration",
        inputSchema: {
          type: "object",
          properties: {
            name: {
              type: "string",
              description: "Name for the bot instance",
            },
            config: {
              type: "object",
              description:
                "Bot configuration including type (e.g., scheduled/bot-name), version, and bot-specific settings",
            },
          },
          required: ["name", "config"],
        },
      },
      {
        name: "bot_update",
        description: "Update an existing bot instance configuration",
        inputSchema: {
          type: "object",
          properties: {
            bot_id: {
              type: "string",
              description: "The bot instance ID to update",
            },
            name: {
              type: "string",
              description: "New name for the bot instance (optional)",
            },
            config: {
              type: "object",
              description: "Updated bot configuration",
            },
          },
          required: ["bot_id", "config"],
        },
      },
      {
        name: "bot_delete",
        description: "Delete a bot instance",
        inputSchema: {
          type: "object",
          properties: {
            bot_id: {
              type: "string",
              description: "The bot instance ID to delete",
            },
          },
          required: ["bot_id"],
        },
      },

      // Logs Tools
      {
        name: "logs_get",
        description: "Get execution logs for a bot instance",
        inputSchema: {
          type: "object",
          properties: {
            bot_id: {
              type: "string",
              description: "The bot instance ID",
            },
            date: {
              type: "string",
              description: "Date in YYYYMMDD format (optional)",
            },
            date_range: {
              type: "string",
              description:
                "Date range in YYYYMMDD-YYYYMMDD format (optional, overrides date)",
            },
            limit: {
              type: "number",
              description:
                "Maximum number of log entries (default: 100, max: 500)",
            },
          },
          required: ["bot_id"],
        },
      },
      {
        name: "logs_summary",
        description:
          "Get a summary of log statistics for a bot (error counts, date range, etc.)",
        inputSchema: {
          type: "object",
          properties: {
            bot_id: {
              type: "string",
              description: "The bot instance ID",
            },
          },
          required: ["bot_id"],
        },
      },

      // Custom Bot Tools
      {
        name: "custom_bot_list",
        description: "List all available custom bots in the marketplace",
        inputSchema: {
          type: "object",
          properties: {},
          required: [],
        },
      },
      {
        name: "custom_bot_get",
        description: "Get details of a specific custom bot",
        inputSchema: {
          type: "object",
          properties: {
            name: {
              type: "string",
              description: "The custom bot name",
            },
            version: {
              type: "string",
              description: "Version to retrieve (optional, defaults to latest)",
            },
          },
          required: ["name"],
        },
      },
      {
        name: "custom_bot_schema",
        description:
          "Get the JSON schema for configuring a custom bot (use this to understand required configuration)",
        inputSchema: {
          type: "object",
          properties: {
            name: {
              type: "string",
              description: "The custom bot name",
            },
            version: {
              type: "string",
              description: "Version to retrieve schema for (optional)",
            },
          },
          required: ["name"],
        },
      },

    ];
  }
}
