import { plainToInstance } from "class-transformer";
import { validate } from "class-validator";
import { GetLogsQueryDto } from "../dto/get-logs-query.dto";

async function validateQuery(query: Record<string, string>) {
  const dto = plainToInstance(GetLogsQueryDto, query);
  const errors = await validate(dto);
  return { dto, errors };
}

describe("GetLogsQueryDto", () => {
  describe("numeric query params", () => {
    it.each([
      ["lookbackDays", "30", 30],
      ["limit", "100", 100],
      ["offset", "0", 0],
    ])("accepts %s=%s", async (field, value, expected) => {
      const { dto, errors } = await validateQuery({ [field]: value });

      expect(errors).toHaveLength(0);
      expect((dto as any)[field]).toBe(expected);
    });

    it.each([
      ["lookbackDays", "30days"],
      ["lookbackDays", "1.5"],
      ["limit", "50abc"],
      ["offset", "2.5"],
    ])("rejects malformed %s=%s", async (field, value) => {
      const { errors } = await validateQuery({ [field]: value });

      expect(errors.length).toBeGreaterThan(0);
      expect(errors[0].property).toBe(field);
    });

    it("rejects an offset above 10000", async () => {
      const { errors } = await validateQuery({ offset: "10001" });

      expect(errors[0].property).toBe("offset");
    });

    it("rejects lookbackDays outside 1-90", async () => {
      expect(
        (await validateQuery({ lookbackDays: "0" })).errors.length,
      ).toBeGreaterThan(0);
      expect(
        (await validateQuery({ lookbackDays: "91" })).errors.length,
      ).toBeGreaterThan(0);
    });
  });

  describe("before cursor", () => {
    it.each(["20260401:4", "1792713599999000000-3"])(
      "accepts before=%s",
      async (before) => {
        const { dto, errors } = await validateQuery({ before });

        expect(errors).toHaveLength(0);
        expect(dto.before).toBe(before);
      },
    );

    it("rejects a before longer than any entry key", async () => {
      const { errors } = await validateQuery({ before: "1".repeat(65) });

      expect(errors[0].property).toBe("before");
    });
  });
});
