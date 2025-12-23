import { Injectable } from "@nestjs/common";
import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
} from "@modelcontextprotocol/sdk/types.js";
import { PinoLogger } from "nestjs-pino";
import {
  MCP_TOOL_NAMES,
  BotDeployInput,
  BotUpdateInput,
  BotDeleteInput,
  BotGetInput,
  LogsGetInput,
  CustomBotGetInput,
  CustomBotSchemaInput,
} from "./mcp.types";
import { BotRepository } from "@/bot/bot.repository";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { LogsService } from "@/logs/logs.service";
import { ApiKeyService } from "@/api-key/api-key.service";

@Injectable()
export class McpService {
  private server: Server;

  constructor(
    private readonly botRepository: BotRepository,
    private readonly customBotService: CustomBotService,
    private readonly logsService: LogsService,
    private readonly apiKeyService: ApiKeyService,
    private readonly logger: PinoLogger,
  ) {
    this.initializeServer();
  }

  private initializeServer() {
    this.server = new Server(
      {
        name: "the0-mcp",
        version: "1.0.0",
      },
      {
        capabilities: {
          tools: {},
        },
      },
    );

    this.registerToolHandlers();
  }

  private registerToolHandlers() {
    // Register tool list handler
    this.server.setRequestHandler(ListToolsRequestSchema, async () => {
      return {
        tools: this.getToolDefinitions(),
      };
    });

    // Register tool call handler
    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;
      return this.handleToolCall(name, args || {});
    });
  }

  private getToolDefinitions() {
    return [
      // Auth Tools
      {
        name: MCP_TOOL_NAMES.AUTH_STATUS,
        description: "Check if the API key is valid and get connection status",
        inputSchema: {
          type: "object" as const,
          properties: {},
          required: [],
        },
      },

      // Bot Instance Tools
      {
        name: MCP_TOOL_NAMES.BOT_LIST,
        description:
          "List all deployed bot instances for the authenticated user",
        inputSchema: {
          type: "object" as const,
          properties: {},
          required: [],
        },
      },
      {
        name: MCP_TOOL_NAMES.BOT_GET,
        description: "Get details of a specific bot instance",
        inputSchema: {
          type: "object" as const,
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
        name: MCP_TOOL_NAMES.BOT_DEPLOY,
        description: "Deploy a new bot instance with the given configuration",
        inputSchema: {
          type: "object" as const,
          properties: {
            config: {
              type: "object",
              description:
                "Bot configuration including name, type (e.g., scheduled/bot-name), version, and bot-specific settings",
            },
          },
          required: ["config"],
        },
      },
      {
        name: MCP_TOOL_NAMES.BOT_UPDATE,
        description: "Update an existing bot instance configuration",
        inputSchema: {
          type: "object" as const,
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
        name: MCP_TOOL_NAMES.BOT_DELETE,
        description: "Delete a bot instance",
        inputSchema: {
          type: "object" as const,
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
        name: MCP_TOOL_NAMES.LOGS_GET,
        description: "Get execution logs for a bot instance",
        inputSchema: {
          type: "object" as const,
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
              description: "Maximum number of log entries (default: 100, max: 500)",
            },
          },
          required: ["bot_id"],
        },
      },
      {
        name: MCP_TOOL_NAMES.LOGS_SUMMARY,
        description:
          "Get a summary of log statistics for a bot (error counts, date range, etc.)",
        inputSchema: {
          type: "object" as const,
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
        name: MCP_TOOL_NAMES.CUSTOM_BOT_LIST,
        description: "List all available custom bots in the marketplace",
        inputSchema: {
          type: "object" as const,
          properties: {},
          required: [],
        },
      },
      {
        name: MCP_TOOL_NAMES.CUSTOM_BOT_GET,
        description: "Get details of a specific custom bot",
        inputSchema: {
          type: "object" as const,
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
        name: MCP_TOOL_NAMES.CUSTOM_BOT_SCHEMA,
        description:
          "Get the JSON schema for configuring a custom bot (use this to understand required configuration)",
        inputSchema: {
          type: "object" as const,
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

  async handleToolCall(
    name: string,
    args: Record<string, unknown>,
    userId?: string,
  ): Promise<{ content: Array<{ type: string; text: string }>; isError?: boolean }> {
    try {
      let result: unknown;

      switch (name) {
        case MCP_TOOL_NAMES.AUTH_STATUS:
          result = { valid: true, message: "API key is valid" };
          break;

        case MCP_TOOL_NAMES.BOT_LIST:
          result = await this.handleBotList(userId);
          break;

        case MCP_TOOL_NAMES.BOT_GET:
          result = await this.handleBotGet(
            args as unknown as BotGetInput,
            userId,
          );
          break;

        case MCP_TOOL_NAMES.BOT_DEPLOY:
          result = await this.handleBotDeploy(
            args as unknown as BotDeployInput,
            userId,
          );
          break;

        case MCP_TOOL_NAMES.BOT_UPDATE:
          result = await this.handleBotUpdate(
            args as unknown as BotUpdateInput,
            userId,
          );
          break;

        case MCP_TOOL_NAMES.BOT_DELETE:
          result = await this.handleBotDelete(
            args as unknown as BotDeleteInput,
            userId,
          );
          break;

        case MCP_TOOL_NAMES.LOGS_GET:
          result = await this.handleLogsGet(
            args as unknown as LogsGetInput,
            userId,
          );
          break;

        case MCP_TOOL_NAMES.LOGS_SUMMARY:
          result = await this.handleLogsSummary(
            args as unknown as { bot_id: string },
            userId,
          );
          break;

        case MCP_TOOL_NAMES.CUSTOM_BOT_LIST:
          result = await this.handleCustomBotList();
          break;

        case MCP_TOOL_NAMES.CUSTOM_BOT_GET:
          result = await this.handleCustomBotGet(
            args as unknown as CustomBotGetInput,
          );
          break;

        case MCP_TOOL_NAMES.CUSTOM_BOT_SCHEMA:
          result = await this.handleCustomBotSchema(
            args as unknown as CustomBotSchemaInput,
          );
          break;

        default:
          return {
            content: [{ type: "text", text: `Unknown tool: ${name}` }],
            isError: true,
          };
      }

      return {
        content: [
          {
            type: "text",
            text: JSON.stringify(result, null, 2),
          },
        ],
      };
    } catch (error) {
      this.logger.error({ error, tool: name }, "MCP tool execution failed");
      return {
        content: [
          {
            type: "text",
            text: `Error: ${error instanceof Error ? error.message : "Unknown error"}`,
          },
        ],
        isError: true,
      };
    }
  }

  // Bot Handlers
  private async handleBotList(userId?: string) {
    if (!userId) {
      throw new Error("Authentication required");
    }
    const result = await this.botRepository.findAll(userId);
    if (!result.success) {
      throw new Error(result.error || "Failed to list bots");
    }
    // Return summary fields only to reduce context size
    return (result.data || []).map((bot) => ({
      id: bot.id,
      name: bot.name,
      type: bot.config?.type || "unknown",
      version: bot.config?.version || "unknown",
      createdAt: bot.createdAt,
      updatedAt: bot.updatedAt,
    }));
  }

  private async handleBotGet(input: BotGetInput, userId?: string) {
    if (!userId) {
      throw new Error("Authentication required");
    }
    const result = await this.botRepository.findOne(userId, input.bot_id);
    if (!result.success) {
      throw new Error(result.error || "Bot not found");
    }
    return result.data;
  }

  private async handleBotDeploy(input: BotDeployInput, userId?: string) {
    if (!userId) {
      throw new Error("Authentication required");
    }

    // Extract fields from config
    const configName = input.config?.name as string;
    const configType = input.config?.type as string;
    const configVersion = input.config?.version as string;

    if (!configName) {
      throw new Error("Config must include 'name' field");
    }

    if (!configType || !configVersion) {
      throw new Error("Config must include 'type' and 'version' fields");
    }

    // Extract custom bot name from type (e.g., "scheduled/alpaca-mixture-of-experts" -> "alpaca-mixture-of-experts")
    const customBotName = configType.includes("/")
      ? configType.split("/")[1]
      : configType;

    // Look up the custom bot to get its ID
    const customBotResult =
      await this.customBotService.getGlobalSpecificVersion(
        customBotName,
        configVersion,
      );

    if (!customBotResult.success || !customBotResult.data) {
      throw new Error(
        `Custom bot '${customBotName}' version '${configVersion}' not found`,
      );
    }

    const result = await this.botRepository.create({
      name: configName,
      config: input.config,
      userId,
      topic: "the0-scheduled-custom-bot",
      customBotId: customBotResult.data.id,
    });
    if (!result.success) {
      throw new Error(result.error || "Failed to deploy bot");
    }
    return {
      message: "Bot deployed successfully",
      bot: {
        id: result.data.id,
        name: result.data.name,
        type: result.data.config?.type,
        version: result.data.config?.version,
      },
    };
  }

  private async handleBotUpdate(input: BotUpdateInput, userId?: string) {
    if (!userId) {
      throw new Error("Authentication required");
    }
    const updateData: Record<string, unknown> = { config: input.config };
    if (input.name) {
      updateData.name = input.name;
    }
    const result = await this.botRepository.update(
      userId,
      input.bot_id,
      updateData,
    );
    if (!result.success) {
      throw new Error(result.error || "Failed to update bot");
    }
    return {
      message: "Bot updated successfully",
      bot: {
        id: result.data.id,
        name: result.data.name,
      },
    };
  }

  private async handleBotDelete(input: BotDeleteInput, userId?: string) {
    if (!userId) {
      throw new Error("Authentication required");
    }
    const result = await this.botRepository.remove(userId, input.bot_id);
    if (!result.success) {
      throw new Error(result.error || "Failed to delete bot");
    }
    return { message: "Bot deleted successfully", bot_id: input.bot_id };
  }

  // Logs Handlers
  private async handleLogsGet(input: LogsGetInput, userId?: string) {
    if (!userId) {
      throw new Error("Authentication required");
    }
    // Cap limit to prevent context overflow
    const limit = Math.min(input.limit || 100, 500);
    const result = await this.logsService.getLogs(input.bot_id, {
      date: input.date,
      dateRange: input.date_range,
      limit,
      offset: 0,
    });
    if (!result.success) {
      throw new Error(result.error || "Failed to get logs");
    }
    return {
      bot_id: input.bot_id,
      count: result.data?.length || 0,
      logs: result.data || [],
    };
  }

  private async handleLogsSummary(input: { bot_id: string }, userId?: string) {
    if (!userId) {
      throw new Error("Authentication required");
    }
    // Get recent logs to analyze - use today's date as default
    const today = new Date();
    const todayStr =
      today.getFullYear().toString() +
      (today.getMonth() + 1).toString().padStart(2, "0") +
      today.getDate().toString().padStart(2, "0");

    const result = await this.logsService.getLogs(input.bot_id, {
      date: todayStr,
      limit: 100,
      offset: 0,
    });
    if (!result.success) {
      throw new Error(result.error || "Failed to get logs for summary");
    }

    const logs = result.data || [];
    const errorCount = logs.filter(
      (log) =>
        log.content?.toLowerCase().includes("error") ||
        log.content?.toLowerCase().includes("failed"),
    ).length;

    return {
      bot_id: input.bot_id,
      total_entries: logs.length,
      error_count: errorCount,
      date_range: {
        start: logs.length > 0 ? logs[logs.length - 1].date : null,
        end: logs.length > 0 ? logs[0].date : null,
      },
      last_entry: logs.length > 0 ? logs[0] : null,
    };
  }

  // Custom Bot Handlers
  private async handleCustomBotList() {
    const result = await this.customBotService.getAllGlobalCustomBots();
    if (!result.success) {
      throw new Error(result.error || "Failed to list custom bots");
    }
    // Return summary fields only
    return (result.data || []).map((bot) => ({
      name: bot.name,
      latestVersion: bot.latestVersion,
      type: bot.versions?.[0]?.config?.type || "unknown",
      description: bot.versions?.[0]?.config?.description || "",
      author: bot.versions?.[0]?.config?.author || "unknown",
      createdAt: bot.createdAt,
    }));
  }

  private async handleCustomBotGet(input: CustomBotGetInput) {
    let result;
    if (input.version) {
      result = await this.customBotService.getGlobalSpecificVersion(
        input.name,
        input.version,
      );
    } else {
      result = await this.customBotService.getGlobalLatestVersion(input.name);
    }

    if (!result.success) {
      throw new Error(result.error || "Custom bot not found");
    }

    const bot = result.data;
    return {
      name: bot.name,
      version: bot.version,
      description: bot.config?.description,
      type: bot.config?.type,
      author: bot.config?.author,
      runtime: bot.config?.runtime,
      entrypoints: bot.config?.entrypoints,
      createdAt: bot.createdAt,
      updatedAt: bot.updatedAt,
    };
  }

  private async handleCustomBotSchema(input: CustomBotSchemaInput) {
    let result;
    if (input.version) {
      result = await this.customBotService.getGlobalSpecificVersion(
        input.name,
        input.version,
      );
    } else {
      result = await this.customBotService.getGlobalLatestVersion(input.name);
    }

    if (!result.success) {
      throw new Error(result.error || "Custom bot not found");
    }

    return {
      name: result.data.name,
      version: result.data.version,
      schema: result.data.config?.schema?.bot || {},
    };
  }

  // Expose server for transport
  getServer(): Server {
    return this.server;
  }
}
