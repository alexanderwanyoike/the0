import { IsOptional, IsString, IsInt, IsIn, Min, Max } from "class-validator";
import { Transform } from "class-transformer";

// Number() (unlike parseInt) rejects trailing garbage ("30days" -> NaN) and
// preserves decimals ("1.5" -> 1.5) so @IsInt can reject both.
const toStrictNumber = ({ value }: { value: string }) => Number(value);

export class GetLogsQueryDto {
  @IsOptional()
  @IsString()
  date?: string;

  @IsOptional()
  @IsString()
  dateRange?: string;

  @IsOptional()
  @Transform(toStrictNumber)
  @IsInt()
  @Min(1)
  @Max(90)
  lookbackDays?: number;

  @IsOptional()
  @Transform(toStrictNumber)
  @IsInt()
  @Min(1)
  @Max(2000)
  limit?: number;

  @IsOptional()
  @Transform(toStrictNumber)
  @IsInt()
  @Min(0)
  offset?: number;

  @IsOptional()
  @IsString()
  @IsIn(["all", "metrics"])
  type?: "all" | "metrics";

  @IsOptional()
  @IsString()
  @IsIn(["asc", "desc"])
  sort?: "asc" | "desc";
}
