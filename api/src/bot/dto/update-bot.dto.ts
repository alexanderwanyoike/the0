import { IsObject, IsOptional, IsString } from "class-validator";
import { BotConfig } from "../../database/schema/bots";

export class UpdateBotDto {
  @IsOptional()
  @IsString()
  name?: string;

  @IsOptional()
  @IsObject()
  config?: BotConfig;
}
