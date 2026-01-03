import {
  Controller,
  Get,
  Delete,
  Param,
  UseGuards,
  BadRequestException,
  NotFoundException,
} from "@nestjs/common";
import { BotStateService } from "./bot-state.service";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";

@Controller("bots/:botId/state")
@UseGuards(AuthCombinedGuard)
export class BotStateController {
  constructor(private readonly botStateService: BotStateService) {}

  /**
   * List all state keys for a bot.
   * GET /bots/:botId/state
   */
  @Get()
  async listKeys(@Param("botId") botId: string) {
    const result = await this.botStateService.listKeys(botId);

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
      message: "State keys retrieved successfully",
    };
  }

  /**
   * Get a specific state value.
   * GET /bots/:botId/state/:key
   */
  @Get(":key")
  async getKey(@Param("botId") botId: string, @Param("key") key: string) {
    const result = await this.botStateService.getKey(botId, key);

    if (!result.success) {
      if (result.error?.includes("not found")) {
        throw new NotFoundException(result.error);
      }
      if (result.error?.includes("access denied")) {
        throw new NotFoundException("Bot not found or access denied");
      }
      throw new BadRequestException(result.error);
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
  async deleteKey(@Param("botId") botId: string, @Param("key") key: string) {
    const result = await this.botStateService.deleteKey(botId, key);

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
  async clearState(@Param("botId") botId: string) {
    const result = await this.botStateService.clearState(botId);

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
      data: { cleared: result.data },
      message: "Bot state cleared successfully",
    };
  }
}
