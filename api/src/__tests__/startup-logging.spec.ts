import { spawnSync } from "child_process";
import path from "path";

function startApi(failure?: "secret" | "migration" | "listen", level = "info") {
  const script = `
    const Module = require("module");
    const originalLoad = Module._load;
    const app = {
      get: () => ({ log() {} }),
      useLogger() {}, enableCors() {}, useGlobalPipes() {},
      async listen() {
        if (process.env.STARTUP_FAILURE === "listen") throw new Error("listen failed");
        process.stderr.write("listening\\n");
      },
    };
    Module._load = function (name, ...rest) {
      if (name === "@nestjs/core") return { NestFactory: {
        create: async () => app,
      }};
      if (name === "./app.module") return { AppModule: class {} };
      if (name === "./database/migrate") return { runMigrations: async () => {
        process.stderr.write("migrating\\n");
        if (process.env.STARTUP_FAILURE === "migration") throw new Error("migration failed");
      }};
      if (name === "./swagger/swagger.setup") return { setupSwagger: () => false };
      return originalLoad.call(this, name, ...rest);
    };
    require(${JSON.stringify(path.resolve(__dirname, "../main.ts"))});
  `;
  return spawnSync(
    process.execPath,
    ["-r", require.resolve("ts-node/register/transpile-only"), "-e", script],
    {
      cwd: path.resolve(__dirname, "../.."),
      encoding: "utf8",
      timeout: 15000,
      env: {
        ...process.env,
        NODE_ENV: "production",
        LOG_LEVEL: level,
        JWT_SECRET: failure === "secret" ? "" : "test-startup-secret",
        STARTUP_FAILURE: failure || "",
      },
    },
  );
}

describe("API startup logging", () => {
  it.each([undefined, "secret", "migration", "listen"] as const)(
    "keeps bootstrap output as JSON for the %s startup case",
    (failure) => {
      const result = startApi(failure);
      expect(result.error).toBeUndefined();
      expect(result.status).toBe(failure ? 1 : 0);
      const records = result.stdout
        .trim()
        .split("\n")
        .map((line) => JSON.parse(line));
      expect(records.every((record) => record.name === "bootstrap")).toBe(true);
      expect(result.stdout).not.toContain("test-startup-secret");
      if (failure === "secret") {
        expect(records).toHaveLength(1);
        expect(records[0]).toMatchObject({
          level: 60,
          msg: expect.stringContaining("JWT_SECRET"),
        });
        expect(result.stderr).not.toContain("migrating");
      } else {
        expect(records[0]).toMatchObject({
          level: 30,
          msg: "Running database migrations...",
        });
        if (failure !== "migration") {
          expect(records[1]).toMatchObject({
            level: 30,
            msg: "Database migrations completed",
          });
        }
        if (failure) {
          expect(records[records.length - 1]).toMatchObject({
            level: 50,
            msg: "Failed to start the0 API",
            err: {
              type: "Error",
              message: `${failure} failed`,
              stack: expect.any(String),
            },
          });
          expect(result.stderr).not.toContain("listening");
        } else {
          expect(records).toHaveLength(2);
          expect(result.stderr).toContain("listening");
        }
      }
    },
  );

  it("respects the configured log level before Nest starts", () => {
    const result = startApi(undefined, "error");
    expect(result.error).toBeUndefined();
    expect(result.status).toBe(0);
    expect(result.stdout).toBe("");
    expect(result.stderr).toContain("listening");
  });
});
