import { IsNotEmpty, IsObject, IsString } from 'class-validator';

export class CreateBacktestDto {
  @IsNotEmpty()
  @IsString()
  name: string;

  @IsNotEmpty()
  @IsObject()
  config: any;
}
