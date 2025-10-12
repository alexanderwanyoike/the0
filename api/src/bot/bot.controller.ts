import {
  Controller,
  Get,
  Post,
  Body,
  Param,
  Delete,
  UseGuards,
  Put,
  BadRequestException,
  NotFoundException,
} from "@nestjs/common";
import { BotService } from "./bot.service";
import { CreateBotDto } from "./dto/create-bot.dto";
import { UpdateBotDto } from "./dto/update-bot.dto";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";

@Controller("bot")
@UseGuards(AuthCombinedGuard)
export class BotController {
  constructor(private readonly botService: BotService) {}

  @Post()
  async create(@Body() createBotDto: CreateBotDto) {
    const result = await this.botService.create(createBotDto);
    if (!result.success) {
      throw new BadRequestException(result.error);
    }
    return result.data;
  }

  @Get()
  async findAll() {
    const result = await this.botService.findAll();
    if (!result.success) {
      throw new NotFoundException(result.error);
    }
    return result.data;
  }

  @Get(":id")
  async findOne(@Param("id") id: string) {
    const result = await this.botService.findOne(id);
    if (!result.success) {
      throw new NotFoundException(result.error);
    }
    return result.data;
  }

  @Put(":id")
  async update(@Param("id") id: string, @Body() updateBotDto: UpdateBotDto) {
    const result = await this.botService.update(id, updateBotDto);
    if (!result.success) {
      throw new BadRequestException(result.error);
    }
    return result.data;
  }

  @Delete(":id")
  async remove(@Param("id") id: string) {
    const result = await this.botService.remove(id);
    if (!result.success) {
      throw new BadRequestException(result.error);
    }
  }
}
