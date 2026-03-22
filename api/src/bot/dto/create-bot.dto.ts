import { IsNotEmpty, IsObject, IsString } from "class-validator";
import { BotConfig } from "../../database/schema/bots";

export class CreateBotDto {
  @IsNotEmpty()
  @IsString()
  name: string;

  @IsNotEmpty()
  @IsObject()
  config: BotConfig;
}
