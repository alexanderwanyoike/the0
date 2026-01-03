import {
  Controller,
  Get,
  Delete,
  Param,
  UseGuards,
  BadRequestException,
  NotFoundException,
} from "@nestjs/common";
import { ApiTags, ApiOperation, ApiResponse, ApiParam } from "@nestjs/swagger";
import { BotStateService, BotStateErrorCode } from "./bot-state.service";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";

@ApiTags("Bot State")
@Controller("bots/:botId/state")
@UseGuards(AuthCombinedGuard)
export class BotStateController {
  constructor(private readonly botStateService: BotStateService) {}

  /**
   * List all state keys for a bot.
   * GET /bots/:botId/state
   */
  @Get()
  @ApiOperation({ summary: "List all state keys for a bot" })
  @ApiParam({ name: "botId", description: "Bot identifier" })
  @ApiResponse({ status: 200, description: "State keys retrieved successfully" })
  @ApiResponse({ status: 404, description: "Bot not found" })
  async listKeys(@Param("botId") botId: string) {
    const result = await this.botStateService.listKeys(botId);

    if (!result.success) {
      switch (result.error?.code) {
        case BotStateErrorCode.BOT_NOT_FOUND:
          throw new NotFoundException("Bot not found or access denied");
        default:
          throw new BadRequestException("Failed to list state keys");
      }
    }

    return {
      success: true,
      data: result.data,
      message: "State keys retrieved successfully",
    };
  }

  /**
   * Get a specific state value.
   * GET /bots/:botId/state/:key
   */
  @Get(":key")
  @ApiOperation({ summary: "Get a specific state value" })
  @ApiParam({ name: "botId", description: "Bot identifier" })
  @ApiParam({ name: "key", description: "State key name" })
  @ApiResponse({ status: 200, description: "State value retrieved successfully" })
  @ApiResponse({ status: 400, description: "Invalid state key" })
  @ApiResponse({ status: 404, description: "Bot or state key not found" })
  async getKey(@Param("botId") botId: string, @Param("key") key: string) {
    const result = await this.botStateService.getKey(botId, key);

    if (!result.success) {
      switch (result.error?.code) {
        case BotStateErrorCode.BOT_NOT_FOUND:
        case BotStateErrorCode.KEY_NOT_FOUND:
          throw new NotFoundException("Bot or state key not found");
        case BotStateErrorCode.INVALID_KEY:
          throw new BadRequestException("Invalid state key");
        default:
          throw new BadRequestException("Failed to get state key");
      }
    }

    return {
      success: true,
      data: result.data,
      message: "State value retrieved successfully",
    };
  }

  /**
   * Delete a specific state key.
   * DELETE /bots/:botId/state/:key
   */
  @Delete(":key")
  @ApiOperation({ summary: "Delete a specific state key" })
  @ApiParam({ name: "botId", description: "Bot identifier" })
  @ApiParam({ name: "key", description: "State key name" })
  @ApiResponse({ status: 200, description: "State key deleted successfully" })
  @ApiResponse({ status: 400, description: "Invalid state key" })
  @ApiResponse({ status: 404, description: "Bot not found" })
  async deleteKey(@Param("botId") botId: string, @Param("key") key: string) {
    const result = await this.botStateService.deleteKey(botId, key);

    if (!result.success) {
      switch (result.error?.code) {
        case BotStateErrorCode.BOT_NOT_FOUND:
          throw new NotFoundException("Bot not found or access denied");
        case BotStateErrorCode.INVALID_KEY:
          throw new BadRequestException("Invalid state key");
        default:
          throw new BadRequestException("Failed to delete state key");
      }
    }

    return {
      success: true,
      data: { deleted: result.data },
      message: result.data
        ? "State key deleted successfully"
        : "State key not found",
    };
  }

  /**
   * Clear all state for a bot.
   * DELETE /bots/:botId/state
   */
  @Delete()
  @ApiOperation({ summary: "Clear all state for a bot" })
  @ApiParam({ name: "botId", description: "Bot identifier" })
  @ApiResponse({ status: 200, description: "Bot state cleared successfully" })
  @ApiResponse({ status: 404, description: "Bot not found" })
  async clearState(@Param("botId") botId: string) {
    const result = await this.botStateService.clearState(botId);

    if (!result.success) {
      switch (result.error?.code) {
        case BotStateErrorCode.BOT_NOT_FOUND:
          throw new NotFoundException("Bot not found or access denied");
        default:
          throw new BadRequestException("Failed to clear state");
      }
    }

    return {
      success: true,
      data: { cleared: result.data },
      message: "Bot state cleared successfully",
    };
  }
}
