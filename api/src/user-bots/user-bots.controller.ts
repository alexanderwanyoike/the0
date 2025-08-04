import {
  Controller,
  Get,
  Query,
  Param,
  BadRequestException,
  UseGuards,
  Post,
  HttpCode,
  HttpStatus,
} from '@nestjs/common';
import { UserBotsService } from './user-bots.service';
import { AuthCombinedGuard } from '@/auth/auth-combined.guard';
import { UserBotQueryParams } from './user-bots.types';

@Controller('user-bots')
@UseGuards(AuthCombinedGuard)
export class UserBotsController {
  constructor(private readonly userBotsService: UserBotsService) {}

  @Post('install/:botName')
  @HttpCode(HttpStatus.OK)
  async install(@Param('botName') botName: string) {
    if (!botName) {
      throw new BadRequestException('Bot name is required');
    }

    const result = await this.userBotsService.install(botName);

    if (!result.success) {
      throw new BadRequestException(result.error);
    }

    return {
      success: true,
      message: 'Free bot added to your collection',
    };
  }

  @Get()
  async getUserBots(@Query() query: UserBotQueryParams) {
    const params: UserBotQueryParams = {
      limit: query.limit ? parseInt(query.limit.toString()) : undefined,
      offset: query.offset ? parseInt(query.offset.toString()) : undefined,
    };

    const result = await this.userBotsService.getUserBots(params);

    if (!result.success) {
      throw new BadRequestException(result.error);
    }

    return {
      success: true,
      data: result.data,
    };
  }

  @Get('has/:botName')
  async hasBot(@Param('botName') botName: string) {
    if (!botName) {
      throw new BadRequestException('Bot name is required');
    }

    const result = await this.userBotsService.getCurrentUserHasBot(botName);

    if (!result.success) {
      throw new BadRequestException(result.error);
    }

    return {
      success: true,
      hasBot: result.data,
    };
  }
}
