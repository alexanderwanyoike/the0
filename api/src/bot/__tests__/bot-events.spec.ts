import { Test, TestingModule } from "@nestjs/testing";
import { PinoLogger } from "nestjs-pino";
import { AppModule } from "@/app.module";
import { AdminBootstrapService } from "@/user/admin-bootstrap.service";
import { DeleteBot, DeployBot, UpdateBot } from "../commands/bot-commands";
import { CommandBus } from "@/command-bus/command-bus";
import { appendBotEvents } from "../bot-event-outbox";
import { NatsService } from "@/nats/nats.service";
import { Failure, Ok, Result } from "@/common/result";
import {
  customBotsTable,
  CustomBotConfig,
} from "@/database/schema/custom-bots";
import { BotConfig } from "@/database/schema/bots";
import { createMockLogger } from "@/test/mock-logger";
import { mockActor } from "@/test/mock-actor";
import { TEST_OWNER_ID, useTestDatabase } from "@/test/test-database";

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

const actor = mockActor({
  userId: TEST_OWNER_ID,
  via: "api-key",
  apiKeyId: "key-1",
});
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
  const db = useTestDatabase();
  let moduleRef: TestingModule;
  let nats: FakeNats;
  let commands: CommandBus;

  async function uploadCustomBot(
    name: string,
    version: string,
    type: "scheduled" | "realtime",
  ) {
    const config = customBotConfig(name, version, type);
    await db()
      .insert(customBotsTable)
      .values({
        id: `${name}-${version}`,
        userId: TEST_OWNER_ID,
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
    const result = await commands.execute(
      new DeployBot(actor, { name: "SMA", config }),
    );
    expect(result.success).toBe(true);
    return result.data!.id;
  }

  async function startApi() {
    moduleRef = await Test.createTestingModule({ imports: [AppModule] })
      .overrideProvider(AdminBootstrapService)
      .useValue({})
      .overrideProvider(NatsService)
      .useValue(nats)
      .overrideProvider(PinoLogger)
      .useValue(createMockLogger())
      .compile();
    await moduleRef.init();
    commands = moduleRef.get(CommandBus);
  }

  beforeEach(async () => {
    nats = new FakeNats();
    await startApi();
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

      const result = await commands.execute(
        new UpdateBot(actor, id, { config }),
      );

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

      const result = await commands.execute(new DeleteBot(actor, id));

      expect(result.success).toBe(true);
      await waitFor(() => nats.published.length === 2);
      expect(nats.published[1]).toEqual({
        subject: "the0.bot-schedule.deleted",
        payload: { id, config: { ...config, hasFrontend: false } },
      });
    });
  });

  describe("publish events left over from before a restart", () => {
    it("publishes the outbox's unpublished events when the api starts", async () => {
      await moduleRef.close();
      await appendBotEvents(db(), [
        {
          botId: "bot-1",
          subject: "the0.bot-schedule.deleted",
          payload: { id: "bot-1", config: { symbol: "AAPL" } },
        },
      ]);

      await startApi();

      await waitFor(() => nats.published.length === 1);
      expect(nats.published).toEqual([
        {
          subject: "the0.bot-schedule.deleted",
          payload: { id: "bot-1", config: { symbol: "AAPL" } },
        },
      ]);
    });
  });

  describe("deliver events whose first publish failed", () => {
    beforeEach(async () => {
      await uploadCustomBot("sma-bot", "1.0.0", "scheduled");
      await uploadCustomBot("sma-bot", "1.1.0", "scheduled");
    });

    it("delivers a deployed bot's event after its publish failed", async () => {
      nats.failNextPublish();

      await deploy(scheduledConfig("1.0.0"));

      await waitFor(() => nats.published.length === 1);
      expect(nats.subjects()).toEqual(["the0.bot-schedule.created"]);
    });

    it("delivers an updated bot's event after its publish failed", async () => {
      const id = await deploy(scheduledConfig("1.0.0"));
      await waitFor(() => nats.published.length === 1);
      nats.failNextPublish();

      const result = await commands.execute(
        new UpdateBot(actor, id, {
          config: scheduledConfig("1.1.0"),
        }),
      );

      expect(result.success).toBe(true);
      await waitFor(() => nats.published.length === 2);
      expect(nats.subjects()[1]).toBe("the0.bot-schedule.updated");
    });

    it("delivers a deleted bot's event after its publish failed, so the runtime stops it", async () => {
      const id = await deploy(scheduledConfig("1.0.0"));
      await waitFor(() => nats.published.length === 1);
      nats.failNextPublish();

      const result = await commands.execute(new DeleteBot(actor, id));

      expect(result.success).toBe(true);
      await waitFor(() => nats.published.length === 2);
      expect(nats.subjects()[1]).toBe("the0.bot-schedule.deleted");
    });

    it("lets bot commands succeed while NATS is down and sends their events in commit order once it is back", async () => {
      nats.goDown();

      const first = await deploy(scheduledConfig("1.0.0"));
      const updated = await commands.execute(
        new UpdateBot(actor, first, {
          config: scheduledConfig("1.1.0"),
        }),
      );
      const removed = await commands.execute(new DeleteBot(actor, first));
      const second = await deploy(scheduledConfig("1.0.0", "MSFT"));
      nats.comeBack();

      expect(updated.success).toBe(true);
      expect(removed.success).toBe(true);
      await waitFor(() => nats.published.length === 4);
      expect(
        nats.published.map(({ subject, payload }) => [subject, payload.id]),
      ).toEqual([
        ["the0.bot-schedule.created", first],
        ["the0.bot-schedule.updated", first],
        ["the0.bot-schedule.deleted", first],
        ["the0.bot-schedule.created", second],
      ]);
    });
  });
});
