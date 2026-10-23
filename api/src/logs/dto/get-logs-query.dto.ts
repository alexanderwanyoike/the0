import {
  IsOptional,
  IsString,
  IsInt,
  IsIn,
  Min,
  Max,
  MaxLength,
} from "class-validator";
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

  /**
   * Prefer `before`: an offset shifts when new lines arrive, and every entry
   * it skips is read and held.
   */
  @IsOptional()
  @Transform(toStrictNumber)
  @IsInt()
  @Min(0)
  @Max(10000)
  offset?: number;

  /**
   * An entry key, usually the previous page's `nextCursor`: returns the
   * `limit` entries just older than that entry. Not combinable with `offset`.
   */
  @IsOptional()
  @IsString()
  @MaxLength(64)
  before?: string;

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
