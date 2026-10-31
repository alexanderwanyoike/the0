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
});
