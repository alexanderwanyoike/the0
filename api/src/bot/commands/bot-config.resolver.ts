import { Injectable } from "@nestjs/common";
import * as semver from "semver";
import { appError, AppError } from "@/common/app-error";
import { Failure, Ok, Result } from "@/common/result";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { CustomBot } from "@/custom-bot/custom-bot.types";
import { BotConfig } from "@/database/schema/bots";
import { parseBotType } from "../bot-type";
import { BotValidator } from "../bot.validator";

/** Finds the Custom bot version a Bot config names and validates the config against it. */
@Injectable()
export class BotConfigResolver {
  constructor(
    private readonly customBotService: CustomBotService,
    private readonly botValidator: BotValidator,
  ) {}

  async resolve(config: BotConfig): Promise<Result<CustomBot, AppError>> {
    const { type, version } = config;

    if (!type) {
      return invalid("Bot type is required");
    }

    if (!version) {
      return invalid("Bot version is required");
    }

    const botType = parseBotType(type);
    if (!botType.success) {
      return Failure(botType.error);
    }

    if (!semver.valid(version)) {
      return invalid("Invalid version format. Expected format: x.y.z");
    }

    const customBotResult =
      await this.customBotService.getGlobalSpecificVersion(
        botType.data.customBotName,
        version,
      );
    if (!customBotResult.success) {
      return customBotResult.error.code === "not_found"
        ? invalid(`Bot type '${type}' not found`)
        : Failure(customBotResult.error);
    }

    const customBot = customBotResult.data;
    const validationResult = await this.botValidator.validate(
      config,
      customBot,
    );
    if (!validationResult.success) {
      return invalid(validationResult.error.join(", ") || "Invalid bot config");
    }

    return Ok(customBot);
  }
}

const invalid = (message: string): Result<CustomBot, AppError> =>
  Failure(appError("validation", message));
