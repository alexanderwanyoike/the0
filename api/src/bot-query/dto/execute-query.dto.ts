import {
  IsString,
  IsNotEmpty,
  IsOptional,
  IsObject,
  IsNumber,
} from "class-validator";

export class ExecuteQueryDto {
  @IsString()
  @IsNotEmpty()
  query_path: string;

  @IsOptional()
  @IsObject()
  params?: Record<string, unknown>;

  @IsOptional()
  @IsNumber()
  timeout_sec?: number;
}
