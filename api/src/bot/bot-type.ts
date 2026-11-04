import { appError, AppError } from "@/common/app-error";
import { Failure, Ok, Result } from "@/common/result";
import { CUSTOM_BOT_NAME } from "@/custom-bot/custom-bot-name";

export interface BotType {
  kind: string;
  customBotName: string;
}

const BOT_TYPE = new RegExp(`^(${CUSTOM_BOT_NAME})/(${CUSTOM_BOT_NAME})$`);

export function parseBotType(type: string): Result<BotType, AppError> {
  const match = BOT_TYPE.exec(type);
  return match
    ? Ok({ kind: match[1], customBotName: match[2] })
    : Failure(
        appError(
          "validation",
          "Invalid bot type format. Expected format: type/name",
        ),
      );
}
