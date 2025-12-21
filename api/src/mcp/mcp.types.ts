/**
 * MCP Types for the0 API
 */

// Tool input schemas
export interface BotListInput {
  // No parameters needed
}

export interface BotGetInput {
  bot_id: string;
}

export interface BotDeployInput {
  config: Record<string, unknown>;
}

export interface BotUpdateInput {
  bot_id: string;
  name?: string;
  config: Record<string, unknown>;
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

export interface CustomBotListInput {
  // No parameters needed
}

export interface CustomBotGetInput {
  name: string;
  version?: string;
}

export interface CustomBotSchemaInput {
  name: string;
  version?: string;
}


// Tool response types
export interface BotSummary {
  id: string;
  name: string;
  type: string;
  version: string;
  status?: string;
  createdAt: string;
  updatedAt: string;
}

export interface LogEntry {
  date: string;
  content: string;
}

export interface LogsSummary {
  bot_id: string;
  total_entries: number;
  date_range: {
    start: string;
    end: string;
  };
  error_count: number;
  last_entry?: LogEntry;
}

export interface CustomBotSummary {
  name: string;
  description: string;
  latestVersion: string;
  type: string;
  author: string;
  createdAt: string;
}


// MCP Tool definition helper type
export interface McpToolDefinition {
  name: string;
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

  // Custom Bot
  CUSTOM_BOT_LIST: "custom_bot_list",
  CUSTOM_BOT_GET: "custom_bot_get",
  CUSTOM_BOT_SCHEMA: "custom_bot_schema",
} as const;

export type McpToolName = (typeof MCP_TOOL_NAMES)[keyof typeof MCP_TOOL_NAMES];
