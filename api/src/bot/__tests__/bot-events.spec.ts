import { Test, TestingModule } from "@nestjs/testing";
import { PinoLogger } from "nestjs-pino";
import { PgliteDatabase } from "drizzle-orm/pglite";
import { AppModule } from "@/app.module";
import { AdminBootstrapService } from "@/auth/admin-bootstrap.service";
import { BotService } from "../bot.service";
import { NatsService } from "@/nats/nats.service";
import { Failure, Ok, Result } from "@/common/result";
import { usersTable } from "@/database/schema/users";
import {
  customBotsTable,
  CustomBotConfig,
} from "@/database/schema/custom-bots";
import { BotConfig } from "@/database/schema/bots";
import { createMockLogger } from "@/test/mock-logger";
import { mockActor } from "@/test/mock-actor";
import {
  resetTestDatabase,
  startTestDatabase,
  stopTestDatabase,
} from "@/test/test-database";

jest.mock(
  "@/database/connection",
  () => jest.requireActual("@/test/test-database").connection,
);

interface PublishedEvent {
  subject: string;
  payload: Record<string, unknown>;
}

/** Records what reaches JetStream, encoded the way NatsService encodes it. */
class FakeNats {
  published: PublishedEvent[] = [];
  private reachable = true;
  private failuresLeft = 0;

  goDown() {
    this.reachable = false;
  }

  comeBack() {
    this.reachable = true;
  }

  failNextPublish() {
    this.failuresLeft = 1;
  }

  async publish(
    subject: string,
    payload: Record<string, unknown>,
  ): Promise<Result<void, string>> {
    if (!this.reachable) return Failure("NATS connection not established");
    if (this.failuresLeft > 0) {
      this.failuresLeft--;
      return Failure("TIMEOUT");
    }
    this.published.push({
      subject,
      payload: JSON.parse(JSON.stringify(payload)),
    });
    return Ok(undefined);
  }

  subjects(): string[] {
    return this.published.map(({ subject }) => subject);
  }
}

async function waitFor(condition: () => boolean, timeoutMs = 4000) {
  const deadline = Date.now() + timeoutMs;
  while (!condition()) {
    if (Date.now() > deadline) {
      throw new Error(`Condition not met within ${timeoutMs}ms`);
    }
    await new Promise((resolve) => setTimeout(resolve, 20));
  }
}

const OWNER = "user-1";
const actor = mockActor({ userId: OWNER, via: "api-key", apiKeyId: "key-1" });
const UPLOADED_AT = new Date("2026-01-02T03:04:05.000Z");

function customBotConfig(
  name: string,
  version: string,
  type: "scheduled" | "realtime",
): CustomBotConfig {
  return {
    name,
    description: `${name} strategy`,
    version,
    type,
    runtime: "python3.11",
    author: "owner",
    entrypoints: { bot: "main.py" },
    schema: {
      bot: {
        type: "object",
        properties: { symbol: { type: "string" } },
        required: ["symbol"],
      },
    },
    readme: `# ${name}`,
  };
}

describe("bot events", () => {
  let db: PgliteDatabase;
  let moduleRef: TestingModule;
  let nats: FakeNats;
  let bots: BotService;

  async function uploadCustomBot(
    name: string,
    version: string,
    type: "scheduled" | "realtime",
  ) {
    const config = customBotConfig(name, version, type);
    await db.insert(customBotsTable).values({
      id: `${name}-${version}`,
      userId: OWNER,
      name,
      version,
      config,
      filePath: `custom-bots/${name}/${version}.zip`,
      createdAt: UPLOADED_AT,
      updatedAt: UPLOADED_AT,
    });
    return {
      id: `${name}-${version}`,
      custom: {
        config,
        createdAt: UPLOADED_AT.toISOString(),
        updatedAt: UPLOADED_AT.toISOString(),
        filePath: `custom-bots/${name}/${version}.zip`,
        version,
      },
    };
  }

  function scheduledConfig(version: string, symbol = "AAPL"): BotConfig {
    return {
      name: "SMA",
      type: "scheduled/sma-bot",
      version,
      schedule: "0 * * * *",
      symbol,
    };
  }

  async function deploy(config: BotConfig): Promise<string> {
    const result = await bots.create(actor, { name: "SMA", config });
    expect(result.success).toBe(true);
    return result.data!.id;
  }

  beforeAll(async () => {
    db = await startTestDatabase();
  });

  afterAll(async () => {
    await stopTestDatabase();
  });

  beforeEach(async () => {
    await resetTestDatabase();
    await db.insert(usersTable).values({
      id: OWNER,
      username: "owner",
      email: "owner@example.com",
      passwordHash: "hash",
    });

    nats = new FakeNats();
    moduleRef = await Test.createTestingModule({ imports: [AppModule] })
      .overrideProvider(AdminBootstrapService)
      .useValue({})
      .overrideProvider(NatsService)
      .useValue(nats)
      .overrideProvider(PinoLogger)
      .useValue(createMockLogger())
      .compile();
    await moduleRef.init();
    bots = moduleRef.get(BotService);
  });

  afterEach(async () => {
    await moduleRef.close();
  });

  describe("tell the runtime about each bot command", () => {
    it("announces a deployed scheduled bot with its config and custom bot", async () => {
      const sma = await uploadCustomBot("sma-bot", "1.0.0", "scheduled");
      const config = scheduledConfig("1.0.0");

      const id = await deploy(config);

      await waitFor(() => nats.published.length === 1);
      expect(nats.published).toEqual([
        {
          subject: "the0.bot-schedule.created",
          payload: {
            id,
            config: { ...config, customBotId: sma.id },
            custom: sma.custom,
          },
        },
      ]);
    });

    it("announces a deployed realtime bot on the realtime subject", async () => {
      const grid = await uploadCustomBot("grid-bot", "1.0.0", "realtime");
      const config = {
        name: "Grid",
        type: "realtime/grid-bot",
        version: "1.0.0",
        symbol: "BTC",
      };

      const id = await deploy(config);

      await waitFor(() => nats.published.length === 1);
      expect(nats.published).toEqual([
        {
          subject: "the0.bot.created",
          payload: {
            id,
            config: { ...config, customBotId: grid.id },
            custom: grid.custom,
          },
        },
      ]);
    });

    it("announces an updated bot with the new version's custom bot", async () => {
      await uploadCustomBot("sma-bot", "1.0.0", "scheduled");
      const next = await uploadCustomBot("sma-bot", "1.1.0", "scheduled");
      const id = await deploy(scheduledConfig("1.0.0"));
      await waitFor(() => nats.published.length === 1);
      const config = scheduledConfig("1.1.0", "MSFT");

      const result = await bots.update(actor, id, { config });

      expect(result.success).toBe(true);
      await waitFor(() => nats.published.length === 2);
      expect(nats.published[1]).toEqual({
        subject: "the0.bot-schedule.updated",
        payload: {
          id,
          config: { ...config, customBotId: next.id },
          custom: next.custom,
        },
      });
    });

    it("announces a deleted bot with the config it was stored with", async () => {
      await uploadCustomBot("sma-bot", "1.0.0", "scheduled");
      const config = scheduledConfig("1.0.0");
      const id = await deploy(config);
      await waitFor(() => nats.published.length === 1);

      const result = await bots.remove(actor, id);

      expect(result.success).toBe(true);
      await waitFor(() => nats.published.length === 2);
      expect(nats.published[1]).toEqual({
        subject: "the0.bot-schedule.deleted",
        payload: { id, config: { ...config, hasFrontend: false } },
      });
    });
  });
});
