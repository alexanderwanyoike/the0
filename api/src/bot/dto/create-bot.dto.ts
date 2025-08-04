import { IsNotEmpty, IsObject, IsString } from 'class-validator';

export class CreateBotDto {
  @IsNotEmpty()
  @IsString()
  name: string;

  @IsNotEmpty()
  @IsObject()
  config: any;
}
