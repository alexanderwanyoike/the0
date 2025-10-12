import {
  Controller,
  Get,
  Param,
  Query,
  UseGuards,
  BadRequestException,
  NotFoundException,
} from "@nestjs/common";
import { LogsService } from "./logs.service";
import { GetLogsQueryDto } from "./dto/get-logs-query.dto";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";

@Controller("logs")
@UseGuards(AuthCombinedGuard)
export class LogsController {
  constructor(private readonly logsService: LogsService) {}

  @Get(":botId")
  async getLogs(
    @Param("botId") botId: string,
    @Query() query: GetLogsQueryDto,
  ) {
    const result = await this.logsService.getLogs(botId, {
      date: query.date,
      dateRange: query.dateRange,
      limit: query.limit || 100,
      offset: query.offset || 0,
    });

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
      data: result.data,
      message: "Logs retrieved successfully",
    };
  }
}
