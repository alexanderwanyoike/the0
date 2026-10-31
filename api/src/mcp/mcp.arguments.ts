import { ClassConstructor, plainToInstance } from "class-transformer";
import { IsNotEmpty, IsOptional, IsString, validate } from "class-validator";
import { ExecuteQueryDto } from "@/bot-query/dto/execute-query.dto";
import { UpdateBotDto } from "@/bot/dto/update-bot.dto";
import { appError, AppError } from "@/common/app-error";
import { Failure, Ok, Result } from "@/common/result";

/*
 * Tool arguments, validated with the DTOs REST uses. The fields REST takes
 * from the path have no DTO there, so they are declared here.
 */

export class BotArguments {
  @IsString()
  @IsNotEmpty()
  bot_id: string;
}

export class BotStateKeyArguments extends BotArguments {
  @IsString()
  @IsNotEmpty()
  key: string;
}

export class BotUpdateArguments extends UpdateBotDto {
  @IsString()
  @IsNotEmpty()
  bot_id: string;
}

export class BotQueryArguments extends ExecuteQueryDto {
  @IsString()
  @IsNotEmpty()
  bot_id: string;
}

export class CustomBotArguments {
  @IsString()
  @IsNotEmpty()
  name: string;

  @IsOptional()
  @IsString()
  version?: string;
}

/**
 * Unlike REST, unknown arguments are dropped rather than refused: model
 * clients often send a stray one, and no tool reads it.
 */
export async function validateArguments<T extends object>(
  dto: ClassConstructor<T>,
  args: object,
): Promise<Result<T, AppError>> {
  const instance = plainToInstance(dto, args);
  const errors = await validate(instance, { whitelist: true });
  if (errors.length === 0) return Ok(instance);
  const messages = errors.flatMap((error) =>
    Object.values(error.constraints ?? {}),
  );
  return Failure(appError("validation", messages.join(", ")));
}
