import configuration from "@/config/configuration";

describe("configuration", () => {
  const originalEnv = { ...process.env };

  afterEach(() => {
    process.env = { ...originalEnv };
  });

  describe("LOG_BUCKET", () => {
    it("defaults to bot-logs, the bucket the runtime writes logs to", () => {
      delete process.env.LOG_BUCKET;

      expect(configuration().LOG_BUCKET).toBe("bot-logs");
    });

    it("uses LOG_BUCKET when it is set", () => {
      process.env.LOG_BUCKET = "team-logs";

      expect(configuration().LOG_BUCKET).toBe("team-logs");
    });
  });

  describe("CUSTOM_BOT_UPLOAD_MAX_BYTES", () => {
    it("defaults to two GiB for large vendored bot archives", () => {
      delete process.env.CUSTOM_BOT_UPLOAD_MAX_BYTES;
      const values: Record<string, unknown> = configuration();
      expect(values.CUSTOM_BOT_UPLOAD_MAX_BYTES).toBe(2 * 1024 ** 3);
    });

    it("uses a configured number of bytes", () => {
      process.env.CUSTOM_BOT_UPLOAD_MAX_BYTES = "4096";
      const values: Record<string, unknown> = configuration();
      expect(values.CUSTOM_BOT_UPLOAD_MAX_BYTES).toBe(4096);
    });

    it.each([
      "",
      "0",
      "-1",
      "1.5",
      "NaN",
      "Infinity",
      "unlimited",
      "9007199254740992",
    ])("refuses invalid byte limit %p at startup", (value) => {
      process.env.CUSTOM_BOT_UPLOAD_MAX_BYTES = value;
      expect(() => configuration()).toThrow("CUSTOM_BOT_UPLOAD_MAX_BYTES");
    });
  });
});
