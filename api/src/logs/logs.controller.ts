import {
  Controller,
  Get,
  Param,
  Query,
  Req,
  Res,
  UseGuards,
  BadRequestException,
  NotFoundException,
} from "@nestjs/common";
import {
  ApiBadRequestResponse,
  ApiNotFoundResponse,
  ApiOkResponse,
  ApiOperation,
  ApiParam,
  ApiProduces,
  ApiTags,
} from "@nestjs/swagger";
import { Request, Response } from "express";
import { LogsService } from "./logs.service";
import { LogStreamService } from "./log-stream.service";
import { isValidBotId } from "./bot-id";
import { GetLogsQueryDto } from "./dto/get-logs-query.dto";
import { StreamLogsQueryDto } from "./dto/stream-logs-query.dto";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";
import { AuthenticatedUser } from "@/auth/auth.types";
import { CurrentUser } from "@/auth/current-user.decorator";
import { ApiJwtOrApiKeyAuth } from "@/swagger/api-auth.decorators";

const BOT_ID_PARAM = { name: "botId", description: "Bot instance ID" };

@ApiTags("logs")
@ApiJwtOrApiKeyAuth()
@Controller("logs")
@UseGuards(AuthCombinedGuard)
export class LogsController {
  constructor(
    private readonly logsService: LogsService,
    private readonly logStream: LogStreamService,
  ) {}

  @Get(":botId")
  @ApiOperation({
    summary: "Read a bot's logs",
    description:
      "Pass `date`, `dateRange` or `lookbackDays`. Without a date or range, " +
      "`lookbackDays` returns the newest entries from up to that many days. " +
      "Page to older entries with `before=<nextCursor>`.",
  })
  @ApiParam(BOT_ID_PARAM)
  @ApiBadRequestResponse({ description: "Invalid bot ID or query" })
  @ApiNotFoundResponse({ description: "Bot not found or access denied" })
  async getLogs(
    @Param("botId") botId: string,
    @Query() query: GetLogsQueryDto,
    @CurrentUser() user: AuthenticatedUser,
  ) {
    if (!isValidBotId(botId)) {
      throw new BadRequestException("Invalid bot ID");
    }

    const result = await this.logsService.getLogs(
      botId,
      {
        date: query.date,
        dateRange: query.dateRange,
        lookbackDays: query.lookbackDays,
        limit: query.limit || 100,
        offset: query.offset || 0,
        before: query.before,
        type: query.type,
        sort: query.sort || "desc",
      },
      user.uid,
    );

    if (!result.success) {
      if (
        result.error?.includes("not found") ||
        result.error?.includes("access denied")
      ) {
        throw new NotFoundException(result.error);
      }
      throw new BadRequestException(result.error);
    }

    return {
      success: true,
      data: result.data.entries,
      hasMore: result.data.hasMore,
      nextCursor: result.data.nextCursor,
      message: "Logs retrieved successfully",
    };
  }

  @Get(":botId/stream")
  @ApiOperation({
    summary: "Stream a bot's logs as server-sent events",
    description:
      "Sends today's logs as a `history` event (skipped with " +
      "`history=false`), then each live chunk as an `update` event holding " +
      "an array of entries in the REST shape. `warning` and `error` events " +
      "report degraded or failed streams.",
  })
  @ApiParam(BOT_ID_PARAM)
  @ApiProduces("text/event-stream")
  @ApiOkResponse({ description: "Event stream" })
  async streamLogs(
    @Param("botId") botId: string,
    @Query() query: StreamLogsQueryDto,
    @CurrentUser() user: AuthenticatedUser,
    @Req() req: Request,
    @Res() res: Response,
  ) {
    await this.logStream.stream(botId, user.uid, req, res, {
      history: query.history !== false,
    });
  }
}
