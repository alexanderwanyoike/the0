import { Injectable } from "@nestjs/common";
import { PinoLogger } from "nestjs-pino";
import { ClassConstructor } from "class-transformer";
import { MCP_TOOL_NAMES, BotDeployInput, LogsGetInput } from "./mcp.types";
import {
  BotArguments,
  BotQueryArguments,
  BotStateKeyArguments,
  BotUpdateArguments,
  CustomBotArguments,
  validateArguments,
} from "./mcp.arguments";
import { CreateBotDto } from "@/bot/dto/create-bot.dto";
import { GetLogsQueryDto } from "@/logs/dto/get-logs-query.dto";
import { Actor } from "@/auth/actor";
import { BotService } from "@/bot/bot.service";
import { DeleteBot, DeployBot, UpdateBot } from "@/bot/commands/bot-commands";
import { CommandBus } from "@/command-bus/command-bus";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { LogsService } from "@/logs/logs.service";
import { utcDay } from "@/logs/log-entry";
import { BotStateService } from "@/bot-state/bot-state.service";
import { BotQueryService } from "@/bot-query/bot-query.service";
import { appError, AppError } from "@/common/app-error";
import { Failure, Ok, Result } from "@/common/result";
import {
  McpToolResult,
  toMcpToolError,
  toMcpToolResult,
} from "./mcp-tool-result";

const LEVEL_PREFIX =
  /^(?:\[[^\]]*\]\s*)?(DEBUG|INFO|WARN(?:ING)?|ERROR|CRITICAL|FATAL):/i;
const ERROR_PREFIXES = new Set(["ERROR", "CRITICAL", "FATAL"]);
// Pino levels as numbers or digit strings, as the frontend maps them.
const ERROR_LEVELS = new Set(["error", "critical", "fatal", "50", "60"]);

function isErrorLevel(level: string | number): boolean {
  return typeof level === "number"
    ? level > 40
    : ERROR_LEVELS.has(level.toLowerCase());
}

/**
 * Entry content is the stored line verbatim, so a JSON line is judged by
 * its level, never by field names such as `failed` or `error_count`.
 */
function isErrorLine(content: string): boolean {
  let parsed: unknown;
  try {
    parsed = JSON.parse(content);
  } catch {
    return isErrorText(content);
  }
  if (parsed === null || typeof parsed !== "object") {
    return isErrorText(content);
  }
  const line = parsed as Record<string, unknown>;
  if (line._metric) return false;
  if (typeof line.level === "string" || typeof line.level === "number") {
    return isErrorLevel(line.level);
  }
  const message = line.message ?? line.msg;
  return typeof message === "string" && isErrorText(message);
}

function isErrorText(text: string): boolean {
  const prefix = LEVEL_PREFIX.exec(text);
  if (prefix) return ERROR_PREFIXES.has(prefix[1].toUpperCase());
  const lower = text.toLowerCase();
  return lower.includes("error") || lower.includes("failed");
}

@Injectable()
export class McpService {
  constructor(
    private readonly botService: BotService,
    private readonly commandBus: CommandBus,
    private readonly botStateService: BotStateService,
    private readonly botQueryService: BotQueryService,
    private readonly customBotService: CustomBotService,
    private readonly logsService: LogsService,
    private readonly logger: PinoLogger,
  ) {}

  async handleToolCall(
    name: string,
    args: Record<string, unknown>,
    actor: Actor,
  ): Promise<McpToolResult> {
    try {
      const result = await this.runTool(name, args, actor);
      return result.success
        ? toMcpToolResult(result.data)
        : toMcpToolError(result.error);
    } catch (error) {
      this.logger.error({ error, tool: name }, "MCP tool execution failed");
      return toMcpToolError(appError("internal", "Tool execution failed"));
    }
  }

  private runTool(
    name: string,
    args: Record<string, unknown>,
    actor: Actor,
  ): Promise<ToolResult> {
    switch (name) {
      case MCP_TOOL_NAMES.AUTH_STATUS:
        return Promise.resolve(
          Ok({ valid: true, message: "Credentials are valid" }),
        );
      case MCP_TOOL_NAMES.BOT_LIST:
        return this.handleBotList(actor);
      case MCP_TOOL_NAMES.BOT_GET:
        return this.withArguments(BotArguments, args, (input) =>
          this.handleBotGet(input, actor),
        );
      case MCP_TOOL_NAMES.BOT_DEPLOY:
        return this.handleBotDeploy(args as unknown as BotDeployInput, actor);
      case MCP_TOOL_NAMES.BOT_UPDATE:
        return this.withArguments(BotUpdateArguments, args, (input) =>
          this.handleBotUpdate(input, actor),
        );
      case MCP_TOOL_NAMES.BOT_DELETE:
        return this.withArguments(BotArguments, args, (input) =>
          this.handleBotDelete(input, actor),
        );
      case MCP_TOOL_NAMES.LOGS_GET:
        return this.withArguments(BotArguments, args, () =>
          this.handleLogsGet(args as unknown as LogsGetInput, actor),
        );
      case MCP_TOOL_NAMES.LOGS_SUMMARY:
        return this.withArguments(BotArguments, args, (input) =>
          this.handleLogsSummary(input, actor),
        );
      case MCP_TOOL_NAMES.BOT_STATE_LIST:
        return this.withArguments(BotArguments, args, (input) =>
          this.handleBotStateList(input, actor),
        );
      case MCP_TOOL_NAMES.BOT_STATE_GET:
        return this.withArguments(BotStateKeyArguments, args, (input) =>
          this.handleBotStateGet(input, actor),
        );
      case MCP_TOOL_NAMES.BOT_QUERY:
        return this.withArguments(BotQueryArguments, args, (input) =>
          this.handleBotQuery(input, actor),
        );
      case MCP_TOOL_NAMES.CUSTOM_BOT_LIST:
        return this.handleCustomBotList();
      case MCP_TOOL_NAMES.CUSTOM_BOT_GET:
        return this.withArguments(CustomBotArguments, args, (input) =>
          this.handleCustomBotGet(input),
        );
      case MCP_TOOL_NAMES.CUSTOM_BOT_SCHEMA:
        return this.withArguments(CustomBotArguments, args, (input) =>
          this.handleCustomBotSchema(input),
        );
      default:
        return Promise.resolve(invalid(`Unknown tool: ${name}`));
    }
  }

  private async withArguments<T extends object>(
    dto: ClassConstructor<T>,
    args: object,
    run: (input: T) => Promise<ToolResult>,
  ): Promise<ToolResult> {
    const input = await validateArguments(dto, args);
    return input.success ? run(input.data) : input;
  }

  // Bot Handlers
  private async handleBotList(actor: Actor): Promise<ToolResult> {
    const result = await this.botService.findAll(actor);
    if (!result.success) return result;
    // Return summary fields only to reduce context size
    return Ok(
      result.data.map((bot) => ({
        id: bot.id,
        name: bot.name,
        type: bot.config?.type || "unknown",
        version: bot.config?.version || "unknown",
        createdAt: bot.createdAt,
        updatedAt: bot.updatedAt,
      })),
    );
  }

  private handleBotGet(input: BotArguments, actor: Actor): Promise<ToolResult> {
    return this.botService.findOne(actor, input.bot_id);
  }

  private async handleBotDeploy(
    input: BotDeployInput,
    actor: Actor,
  ): Promise<ToolResult> {
    // The tool takes the name inside the config, where REST takes it beside.
    const bot = await validateArguments(CreateBotDto, {
      name: input.config?.name,
      config: input.config,
    });
    if (!bot.success) return bot;

    const result = await this.commandBus.execute(
      new DeployBot(actor, { ...bot.data }),
    );
    if (!result.success) return result;
    return Ok({
      message: "Bot deployed successfully",
      bot: {
        id: result.data.id,
        name: result.data.name,
        type: result.data.config?.type,
        version: result.data.config?.version,
      },
    });
  }

  private async handleBotUpdate(
    input: BotUpdateArguments,
    actor: Actor,
  ): Promise<ToolResult> {
    const result = await this.commandBus.execute(
      new UpdateBot(actor, input.bot_id, {
        config: input.config,
        ...(input.name && { name: input.name }),
      }),
    );
    if (!result.success) return result;
    return Ok({
      message: "Bot updated successfully",
      bot: {
        id: result.data.id,
        name: result.data.name,
      },
    });
  }

  private async handleBotDelete(
    input: BotArguments,
    actor: Actor,
  ): Promise<ToolResult> {
    const result = await this.commandBus.execute(
      new DeleteBot(actor, input.bot_id),
    );
    if (!result.success) return result;
    return Ok({ message: "Bot deleted successfully", bot_id: input.bot_id });
  }

  // Logs Handlers
  private async handleLogsGet(
    input: LogsGetInput,
    actor: Actor,
  ): Promise<ToolResult> {
    // An absent key must stay absent: the DTO's number transform turns an
    // explicit undefined into NaN.
    const query = await validateArguments(
      GetLogsQueryDto,
      Object.fromEntries(
        Object.entries({
          date: input.date,
          dateRange: input.date_range,
          limit: input.limit,
        }).filter(([, value]) => value !== undefined),
      ),
    );
    if (!query.success) return query;

    // Cap limit to prevent context overflow
    const limit = Math.min(query.data.limit || 100, 500);
    const result = await this.logsService.getLogs(
      input.bot_id,
      {
        date: query.data.date,
        dateRange: query.data.dateRange,
        limit,
        offset: 0,
      },
      actor.userId,
    );
    if (!result.success) return result;
    const entries = result.data.entries;
    return Ok({
      bot_id: input.bot_id,
      count: entries.length,
      logs: entries,
    });
  }

  private async handleLogsSummary(
    input: BotArguments,
    actor: Actor,
  ): Promise<ToolResult> {
    const result = await this.logsService.getLogs(
      input.bot_id,
      {
        date: utcDay(new Date()),
        limit: 100,
        offset: 0,
      },
      actor.userId,
    );
    if (!result.success) return result;

    const logs = result.data.entries;
    const errorCount = logs.filter((log) => isErrorLine(log.content)).length;

    return Ok({
      bot_id: input.bot_id,
      total_entries: logs.length,
      error_count: errorCount,
      date_range: {
        start: logs.length > 0 ? logs[logs.length - 1].date : null,
        end: logs.length > 0 ? logs[0].date : null,
      },
      last_entry: logs.length > 0 ? logs[0] : null,
    });
  }

  // Bot State Handlers
  private async handleBotStateList(
    input: BotArguments,
    actor: Actor,
  ): Promise<ToolResult> {
    const result = await this.botStateService.listKeys(actor, input.bot_id);
    if (!result.success) return result;
    return Ok({ bot_id: input.bot_id, keys: result.data });
  }

  private async handleBotStateGet(
    input: BotStateKeyArguments,
    actor: Actor,
  ): Promise<ToolResult> {
    const result = await this.botStateService.getKey(
      actor,
      input.bot_id,
      input.key,
    );
    if (!result.success) return result;
    return Ok({ bot_id: input.bot_id, key: input.key, value: result.data });
  }

  // Bot Query Handlers
  private async handleBotQuery(
    input: BotQueryArguments,
    actor: Actor,
  ): Promise<ToolResult> {
    return this.botQueryService.executeQuery(actor, input.bot_id, {
      queryPath: input.query_path,
      params: input.params,
      timeoutSec: input.timeout_sec,
    });
  }

  // Custom Bot Handlers
  private async handleCustomBotList(): Promise<ToolResult> {
    const result = await this.customBotService.getAllGlobalCustomBots();
    if (!result.success) return result;
    // Return summary fields only
    return Ok(
      result.data.map((bot) => ({
        name: bot.name,
        latestVersion: bot.latestVersion,
        type: bot.versions?.[0]?.config?.type || "unknown",
        description: bot.versions?.[0]?.config?.description || "",
        author: bot.versions?.[0]?.config?.author || "unknown",
        createdAt: bot.createdAt,
      })),
    );
  }

  private async handleCustomBotGet(
    input: CustomBotArguments,
  ): Promise<ToolResult> {
    const result = await this.findCustomBot(input);
    if (!result.success) return result;

    const bot = result.data;
    return Ok({
      name: bot.name,
      version: bot.version,
      description: bot.config?.description,
      type: bot.config?.type,
      author: bot.config?.author,
      runtime: bot.config?.runtime,
      entrypoints: bot.config?.entrypoints,
      createdAt: bot.createdAt,
      updatedAt: bot.updatedAt,
    });
  }

  private async handleCustomBotSchema(
    input: CustomBotArguments,
  ): Promise<ToolResult> {
    const result = await this.findCustomBot(input);
    if (!result.success) return result;

    return Ok({
      name: result.data.name,
      version: result.data.version,
      schema: result.data.config?.schema?.bot || {},
    });
  }

  private findCustomBot(input: CustomBotArguments) {
    return input.version
      ? this.customBotService.getGlobalSpecificVersion(
          input.name,
          input.version,
        )
      : this.customBotService.getGlobalLatestVersion(input.name);
  }
}

type ToolResult = Result<unknown, AppError>;

const invalid = (message: string): ToolResult =>
  Failure(appError("validation", message));
