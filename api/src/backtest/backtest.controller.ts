import {
  Controller,
  Get,
  Post,
  Body,
  Param,
  Delete,
  NotFoundException,
  BadRequestException,
  UseGuards,
} from "@nestjs/common";
import { BacktestService } from "./backtest.service";
import { CreateBacktestDto } from "./dto/create-backtest.dto";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";

@Controller("backtest")
@UseGuards(AuthCombinedGuard)
export class BacktestController {
  constructor(private readonly backtestService: BacktestService) {}

  @Post()
  async create(@Body() createBacktestDto: CreateBacktestDto) {
    const result = await this.backtestService.create(createBacktestDto);
    if (!result.success) {
      throw new BadRequestException(result.error);
    }
    return result.data;
  }

  @Get()
  async findAll() {
    const result = await this.backtestService.findAll();
    if (!result.success) {
      throw new NotFoundException(result.error);
    }
    return result.data;
  }

  @Get(":id")
  async findOne(@Param("id") id: string) {
    const result = await this.backtestService.findOne(id);
    if (!result.success) {
      throw new NotFoundException(result.error);
    }
    return result.data;
  }

  @Delete(":id")
  async remove(@Param("id") id: string) {
    const result = await this.backtestService.remove(id);
    if (!result.success) {
      throw new NotFoundException(result.error);
    }
  }

  @Get(":id/logs")
  async getBacktestLogs(@Param("id") id: string) {
    const result = await this.backtestService.getBacktestLogs(id);

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
