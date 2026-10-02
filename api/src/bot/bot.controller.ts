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
import {
  ApiBadRequestResponse,
  ApiNotFoundResponse,
  ApiOperation,
  ApiParam,
  ApiTags,
} from "@nestjs/swagger";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";
import { ApiJwtOrApiKeyAuth } from "@/swagger/api-auth.decorators";

@ApiTags("bots")
@ApiJwtOrApiKeyAuth()
@Controller("bot")
@UseGuards(AuthCombinedGuard)
export class BotController {
  constructor(private readonly botService: BotService) {}

  @Post()
  @ApiOperation({ summary: "Deploy a bot instance from a custom bot" })
  @ApiBadRequestResponse({ description: "Invalid bot config" })
  async create(@Body() createBotDto: CreateBotDto) {
    const result = await this.botService.create(createBotDto);
    if (!result.success) {
      throw new BadRequestException(result.error);
    }
    return result.data;
  }

  @Get()
  @ApiOperation({ summary: "List your bot instances" })
  async findAll() {
    const result = await this.botService.findAll();
    if (!result.success) {
      throw new NotFoundException(result.error);
    }
    return result.data;
  }

  @Get(":id")
  @ApiOperation({ summary: "Get a bot instance" })
  @ApiParam({ name: "id", description: "Bot instance ID" })
  @ApiNotFoundResponse({ description: "Bot not found" })
  async findOne(@Param("id") id: string) {
    const result = await this.botService.findOne(id);
    if (!result.success) {
      throw new NotFoundException(result.error);
    }
    return result.data;
  }

  @Put(":id")
  @ApiOperation({ summary: "Update a bot instance's config" })
  @ApiParam({ name: "id", description: "Bot instance ID" })
  @ApiBadRequestResponse({ description: "Invalid bot config or bot not found" })
  async update(@Param("id") id: string, @Body() updateBotDto: UpdateBotDto) {
    const result = await this.botService.update(id, updateBotDto);
    if (!result.success) {
      throw new BadRequestException(result.error);
    }
    return result.data;
  }

  @Delete(":id")
  @ApiOperation({ summary: "Delete a bot instance" })
  @ApiParam({ name: "id", description: "Bot instance ID" })
  @ApiBadRequestResponse({ description: "Bot could not be deleted" })
  async remove(@Param("id") id: string) {
    const result = await this.botService.remove(id);
    if (!result.success) {
      throw new BadRequestException(result.error);
    }
    return result.data;
  }
}
