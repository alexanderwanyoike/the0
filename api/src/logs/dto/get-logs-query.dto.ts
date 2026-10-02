import { IsOptional, IsString, IsInt, IsIn, Min, Max } from "class-validator";
import { Transform } from "class-transformer";
import { ApiPropertyOptional } from "@nestjs/swagger";

// Number() (unlike parseInt) rejects trailing garbage ("30days" -> NaN) and
// preserves decimals ("1.5" -> 1.5) so @IsInt can reject both.
const toStrictNumber = ({ value }: { value: string }) => Number(value);

export class GetLogsQueryDto {
  /** Single day, YYYYMMDD. */
  @IsOptional()
  @IsString()
  date?: string;

  /** YYYYMMDD-YYYYMMDD, or an ISO datetime range split by `--`. */
  @IsOptional()
  @IsString()
  dateRange?: string;

  @IsOptional()
  @Transform(toStrictNumber)
  @IsInt()
  @Min(1)
  @Max(90)
  lookbackDays?: number;

  /** Defaults to 100. */
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

  /** `metrics` keeps only metric lines. */
  @ApiPropertyOptional({ enum: ["all", "metrics"] })
  @IsOptional()
  @IsString()
  @IsIn(["all", "metrics"])
  type?: "all" | "metrics";

  /** Defaults to desc. */
  @ApiPropertyOptional({ enum: ["asc", "desc"] })
  @IsOptional()
  @IsString()
  @IsIn(["asc", "desc"])
  sort?: "asc" | "desc";
}
