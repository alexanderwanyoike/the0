import { Injectable } from "@nestjs/common";
import * as semver from "semver";
import { Failure, Ok, Result } from "@/common/result";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { CustomBot } from "@/custom-bot/custom-bot.types";
import { BotConfig } from "@/database/schema/bots";
import { BOT_TYPE_PATTERN } from "../bot.constants";
import { BotValidator } from "../bot.validator";

/** Finds the Custom bot version a Bot config names and validates the config against it. */
@Injectable()
export class BotConfigResolver {
  constructor(
    private readonly customBotService: CustomBotService,
    private readonly botValidator: BotValidator,
  ) {}

  async resolve(config: BotConfig): Promise<Result<CustomBot, string>> {
    const { type, version } = config;

    if (!type) {
      return Failure("Bot type is required");
    }

    if (!version) {
      return Failure("Bot version is required");
    }

    if (!BOT_TYPE_PATTERN.test(type)) {
      return Failure("Invalid bot type format. Expected format: type/name");
    }

    if (!semver.valid(version)) {
      return Failure("Invalid version format. Expected format: x.y.z");
    }

    const name = type.split("/")[1];
    if (!name?.trim()) {
      return Failure("Invalid bot type: missing name after '/'");
    }

    const customBotResult =
      await this.customBotService.getGlobalSpecificVersion(name, version);
    if (!customBotResult.success) {
      return Failure(`Bot type '${type}' not found`);
    }

    const customBot = customBotResult.data;
    const validationResult = await this.botValidator.validate(
      config,
      customBot,
    );
    if (!validationResult.success) {
      return Failure(validationResult.error.join(", ") || "Invalid bot config");
    }

    return Ok(customBot);
  }
}
