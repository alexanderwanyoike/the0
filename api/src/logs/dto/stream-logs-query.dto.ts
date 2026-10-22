import { IsBoolean, IsOptional } from "class-validator";
import { Transform } from "class-transformer";
import { ApiPropertyOptional } from "@nestjs/swagger";

const toBoolean = ({ value }: { value: unknown }) =>
  value === "true" ? true : value === "false" ? false : value;

export class StreamLogsQueryDto {
  /** `false` skips the `history` event. Defaults to true. */
  @ApiPropertyOptional({ type: Boolean })
  @IsOptional()
  @Transform(toBoolean)
  @IsBoolean()
  history?: boolean;
}
