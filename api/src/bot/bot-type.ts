import { appError, AppError } from "@/common/app-error";
import { Failure, Ok, Result } from "@/common/result";

export interface BotType {
  kind: string;
  customBotName: string;
}

const BOT_TYPE = /^([a-z0-9]+(?:-[a-z0-9]+)*)\/([a-z0-9]+(?:-[a-z0-9]+)*)$/;

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
