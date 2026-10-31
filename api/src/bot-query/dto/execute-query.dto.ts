import { IsString, IsOptional, IsObject, IsNumber } from "class-validator";

export class ExecuteQueryDto {
  @IsString()
  query_path: string;

  @IsOptional()
  @IsObject()
  params?: Record<string, unknown>;

  @IsOptional()
  @IsNumber()
  timeout_sec?: number;
}
