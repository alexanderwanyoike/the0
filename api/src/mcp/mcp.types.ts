/**
 * MCP Types for the0 API
 */
import { BotConfig } from "@/database/schema/bots";

// Tool input schemas
export interface BotGetInput {
  bot_id: string;
}

export interface BotDeployInput {
  config: BotConfig;
}

export interface BotUpdateInput {
  bot_id: string;
  name?: string;
  config: BotConfig;
}

export interface BotDeleteInput {
  bot_id: string;
}

export interface LogsGetInput {
  bot_id: string;
  date?: string;
  date_range?: string;
  limit?: number;
}

export interface BotStateListInput {
  bot_id: string;
}

export interface BotStateGetInput {
  bot_id: string;
  key: string;
}

export interface BotQueryInput {
  bot_id: string;
  query_path: string;
  params?: Record<string, unknown>;
  timeout_sec?: number;
}

export interface CustomBotGetInput {
  name: string;
  version?: string;
}

export interface CustomBotSchemaInput {
  name: string;
  version?: string;
}

interface McpToolDefinition {
  name: McpToolName;
  description: string;
  inputSchema: {
    type: "object";
    properties: Record<string, unknown>;
    required: string[];
  };
}

// Constants
export const MCP_TOOL_NAMES = {
  // Auth
  AUTH_STATUS: "auth_status",

  // Bot Instance
  BOT_LIST: "bot_list",
  BOT_GET: "bot_get",
  BOT_DEPLOY: "bot_deploy",
  BOT_UPDATE: "bot_update",
  BOT_DELETE: "bot_delete",

  // Logs
  LOGS_GET: "logs_get",
  LOGS_SUMMARY: "logs_summary",

  // Bot State
  BOT_STATE_LIST: "bot_state_list",
  BOT_STATE_GET: "bot_state_get",

  // Bot Query
  BOT_QUERY: "bot_query",

  // Custom Bot
  CUSTOM_BOT_LIST: "custom_bot_list",
  CUSTOM_BOT_GET: "custom_bot_get",
  CUSTOM_BOT_SCHEMA: "custom_bot_schema",
} as const;

type McpToolName = (typeof MCP_TOOL_NAMES)[keyof typeof MCP_TOOL_NAMES];

export const MCP_TOOL_DEFINITIONS: McpToolDefinition[] = [
  // Auth Tools
  {
    name: MCP_TOOL_NAMES.AUTH_STATUS,
    description: "Check if the API key is valid and get connection status",
    inputSchema: {
      type: "object",
      properties: {},
      required: [],
    },
  },

  // Bot Instance Tools
  {
    name: MCP_TOOL_NAMES.BOT_LIST,
    description: "List all deployed bot instances for the authenticated user",
    inputSchema: {
      type: "object",
      properties: {},
      required: [],
    },
  },
  {
    name: MCP_TOOL_NAMES.BOT_GET,
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
    name: MCP_TOOL_NAMES.BOT_DEPLOY,
    description: "Deploy a new bot instance with the given configuration",
    inputSchema: {
      type: "object",
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
    name: MCP_TOOL_NAMES.BOT_DELETE,
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
    name: MCP_TOOL_NAMES.LOGS_GET,
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

  // Bot State Tools
  {
    name: MCP_TOOL_NAMES.BOT_STATE_LIST,
    description:
      "List the persisted state keys for a bot instance (with sizes)",
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
    name: MCP_TOOL_NAMES.BOT_STATE_GET,
    description:
      "Get the value of a specific persisted state key for a bot instance",
    inputSchema: {
      type: "object",
      properties: {
        bot_id: {
          type: "string",
          description: "The bot instance ID",
        },
        key: {
          type: "string",
          description: "The state key name",
        },
      },
      required: ["bot_id", "key"],
    },
  },

  // Bot Query Tools
  {
    name: MCP_TOOL_NAMES.BOT_QUERY,
    description:
      "Execute a query against a running realtime bot's query endpoint",
    inputSchema: {
      type: "object",
      properties: {
        bot_id: {
          type: "string",
          description: "The bot instance ID",
        },
        query_path: {
          type: "string",
          description: "Query path exposed by the bot (e.g. /positions)",
        },
        params: {
          type: "object",
          description: "Query parameters (optional)",
        },
        timeout_sec: {
          type: "number",
          description: "Query timeout in seconds (default: 30)",
        },
      },
      required: ["bot_id", "query_path"],
    },
  },

  // Custom Bot Tools
  {
    name: MCP_TOOL_NAMES.CUSTOM_BOT_LIST,
    description: "List all available custom bots in the marketplace",
    inputSchema: {
      type: "object",
      properties: {},
      required: [],
    },
  },
  {
    name: MCP_TOOL_NAMES.CUSTOM_BOT_GET,
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
    name: MCP_TOOL_NAMES.CUSTOM_BOT_SCHEMA,
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
