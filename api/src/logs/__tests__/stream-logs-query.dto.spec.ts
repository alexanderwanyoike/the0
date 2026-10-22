import { plainToInstance } from "class-transformer";
import { validate } from "class-validator";
import { StreamLogsQueryDto } from "../dto/stream-logs-query.dto";

async function validateQuery(query: Record<string, string>) {
  const dto = plainToInstance(StreamLogsQueryDto, query);
  const errors = await validate(dto);
  return { dto, errors };
}

describe("StreamLogsQueryDto", () => {
  it.each([
    ["false", false],
    ["true", true],
  ])("reads history=%s", async (value, expected) => {
    const { dto, errors } = await validateQuery({ history: value });

    expect(errors).toHaveLength(0);
    expect(dto.history).toBe(expected);
  });

  it("leaves history unset when absent", async () => {
    const { dto, errors } = await validateQuery({});

    expect(errors).toHaveLength(0);
    expect(dto.history).toBeUndefined();
  });

  it("rejects a history value that is not a boolean", async () => {
    const { errors } = await validateQuery({ history: "no" });

    expect(errors[0].property).toBe("history");
  });
});
