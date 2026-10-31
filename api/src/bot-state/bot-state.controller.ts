import {
  Controller,
  Get,
  Delete,
  Param,
  UseGuards,
  BadRequestException,
  NotFoundException,
  PayloadTooLargeException,
  UnprocessableEntityException,
  ConflictException,
} from "@nestjs/common";
import { ApiTags, ApiOperation, ApiResponse, ApiParam } from "@nestjs/swagger";
import { BotStateService } from "./bot-state.service";
import { Actor } from "@/auth/actor";
import { CurrentActor } from "@/auth/actor.decorators";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";
import { ApiJwtOrApiKeyAuth } from "@/swagger/api-auth.decorators";

@ApiTags("bot-state")
@ApiJwtOrApiKeyAuth()
@Controller("bots/:botId/state")
@UseGuards(AuthCombinedGuard)
export class BotStateController {
  constructor(private readonly botStateService: BotStateService) {}

  @Get()
  @ApiOperation({ summary: "List all state keys for a bot" })
  @ApiParam({ name: "botId", description: "Bot identifier" })
  @ApiResponse({
    status: 200,
    description: "State keys retrieved successfully",
  })
  @ApiResponse({ status: 404, description: "Bot not found" })
  async listKeys(@CurrentActor() actor: Actor, @Param("botId") botId: string) {
    const result = await this.botStateService.listKeys(actor, botId);

    if (!result.success) {
      switch (result.error.code) {
        case "not_found":
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

  @Get(":key")
  @ApiOperation({ summary: "Get a specific state value" })
  @ApiParam({ name: "botId", description: "Bot identifier" })
  @ApiParam({ name: "key", description: "State key name" })
  @ApiResponse({
    status: 200,
    description: "State value retrieved successfully",
  })
  @ApiResponse({ status: 400, description: "Invalid state key" })
  @ApiResponse({ status: 404, description: "Bot or state key not found" })
  async getKey(
    @CurrentActor() actor: Actor,
    @Param("botId") botId: string,
    @Param("key") key: string,
  ) {
    const result = await this.botStateService.getKey(actor, botId, key);

    if (!result.success) {
      switch (result.error.code) {
        case "not_found":
          throw new NotFoundException("Bot or state key not found");
        case "validation":
          throw new BadRequestException("Invalid state key");
        case "too_large":
          throw new PayloadTooLargeException(result.error.message);
        case "unprocessable":
          throw new UnprocessableEntityException(
            "State file contains invalid JSON",
          );
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

  @Delete(":key")
  @ApiOperation({ summary: "Delete a specific state key" })
  @ApiParam({ name: "botId", description: "Bot identifier" })
  @ApiParam({ name: "key", description: "State key name" })
  @ApiResponse({ status: 200, description: "State key deleted successfully" })
  @ApiResponse({ status: 400, description: "Invalid state key" })
  @ApiResponse({ status: 404, description: "Bot not found" })
  @ApiResponse({
    status: 409,
    description:
      "Concurrent modification - state was modified by another operation",
  })
  async deleteKey(
    @CurrentActor() actor: Actor,
    @Param("botId") botId: string,
    @Param("key") key: string,
  ) {
    const result = await this.botStateService.deleteKey(actor, botId, key);

    if (!result.success) {
      switch (result.error.code) {
        case "not_found":
          throw new NotFoundException("Bot not found or access denied");
        case "validation":
          throw new BadRequestException("Invalid state key");
        case "conflict":
          throw new ConflictException(
            "State was modified by another operation. Please retry the request.",
          );
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

  @Delete()
  @ApiOperation({ summary: "Clear all state for a bot" })
  @ApiParam({ name: "botId", description: "Bot identifier" })
  @ApiResponse({ status: 200, description: "Bot state cleared successfully" })
  @ApiResponse({ status: 404, description: "Bot not found" })
  async clearState(
    @CurrentActor() actor: Actor,
    @Param("botId") botId: string,
  ) {
    const result = await this.botStateService.clearState(actor, botId);

    if (!result.success) {
      switch (result.error.code) {
        case "not_found":
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
