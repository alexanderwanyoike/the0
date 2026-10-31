import { PinoLogger } from "nestjs-pino";
import { Ok } from "@/common/result";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { CustomBot } from "@/custom-bot/custom-bot.types";
import { BotConfig } from "@/database/schema/bots";
import { createMockLogger } from "@/test/mock-logger";
import { loadContract } from "@/test/contracts";
import { mockBot } from "@/test/mock-bot";
import { BOT_TOPICS, SCHEDULED_BOT_TOPICS } from "../bot.constants";
import { BotEvent } from "../bot-event-outbox";
import { BotEventFactory } from "../commands/bot-event-factory";

const event = (name: string) =>
  loadContract<{ subject: string; payload: unknown }>(`events/${name}.json`);

const REALTIME_BOT = "q3n8v1x7k2m9p4r6t0w5y8z1";
const SCHEDULED_BOT = "h5j9l3n7p1r5t9v3x7z1b5d9";

/**
 * A Catalogue row as the custom bot repository returns it after an upload:
 * the uploaded bot-config.yaml plus the hasFrontend flag the upload adds.
 * Written out here rather than read from the contract, so the payload's
 * custom part is checked against what the api holds, not against itself.
 */
function catalogueRow(
  id: string,
  name: string,
  version: string,
  type: "realtime" | "scheduled",
  entrypoints: CustomBot["config"]["entrypoints"] & { query?: string },
  description: string,
): CustomBot {
  return {
    id,
    name,
    version,
    config: {
      name,
      description,
      version,
      type,
      runtime: "python3.11",
      author: "alex",
      entrypoints,
      schema: {
        bot: {
          type: "object",
          properties: { symbol: { type: "string" } },
          required: ["symbol"],
        },
      },
      readme: `# ${name}`,
      metadata: { tags: ["example"] },
      hasFrontend: false,
    },
    filePath: `u4b8c2d6f0h3j7k1m5n9p2r6/${name}/${name}-${version}.zip`,
    userId: "u4b8c2d6f0h3j7k1m5n9p2r6",
    status: "active",
    createdAt: new Date("2026-10-01T09:30:00.000Z"),
    updatedAt: new Date("2026-10-02T10:45:00.000Z"),
  };
}

const gridTrader = (id: string, version: string) =>
  catalogueRow(
    id,
    "grid-trader",
    version,
    "realtime",
    { bot: "main.py", query: "query.py" },
    "Grid trading strategy",
  );

const smaCrossover = (id: string, version: string) =>
  catalogueRow(
    id,
    "sma-crossover",
    version,
    "scheduled",
    { bot: "main.py" },
    "SMA crossover strategy",
  );

const gridConfig = (version: string, symbol: string): BotConfig => ({
  name: "Grid BTC",
  type: "realtime/grid-trader",
  version,
  symbol,
});

const smaConfig = (
  version: string,
  symbol: string,
  schedule: string,
): BotConfig => ({
  name: "SMA AAPL",
  type: "scheduled/sma-crossover",
  version,
  schedule,
  symbol,
});

/** What NatsService sends: the payload as JSON. */
const onTheWire = (events: BotEvent[]) =>
  events.map(({ subject, payload }) => ({
    subject,
    payload: JSON.parse(JSON.stringify(payload)),
  }));

describe("bot event contracts", () => {
  let customBotService: { getGlobalSpecificVersion: jest.Mock };
  let factory: BotEventFactory;

  beforeEach(() => {
    customBotService = { getGlobalSpecificVersion: jest.fn() };
    factory = new BotEventFactory(
      customBotService as unknown as CustomBotService,
      createMockLogger() as unknown as PinoLogger,
    );
  });

  describe("publish each bot command's event as the contract shows it", () => {
    it.each([
      [
        "bot-created",
        "created",
        REALTIME_BOT,
        gridConfig("1.0.0", "BTC/USD"),
        gridTrader("c7d2f9h4j6l1n3p5r8s0u2w4", "1.0.0"),
      ],
      [
        "bot-updated",
        "updated",
        REALTIME_BOT,
        gridConfig("1.1.0", "ETH/USD"),
        gridTrader("e1g5i9k3m7o1q5s9u3w7y1a5", "1.1.0"),
      ],
      [
        "bot-schedule-created",
        "created",
        SCHEDULED_BOT,
        smaConfig("1.2.0", "AAPL", "0 14 * * 1-5"),
        smaCrossover("f2h6j0l4n8p2r6t0v4x8z2b6", "1.2.0"),
      ],
      [
        "bot-schedule-updated",
        "updated",
        SCHEDULED_BOT,
        smaConfig("1.3.0", "MSFT", "30 15 * * 1-5"),
        smaCrossover("g3i7k1m5o9q3s7u1w5y9a3c7", "1.3.0"),
      ],
    ] as const)(
      "publishes %s",
      (name, command, botId, requestConfig, customBot) => {
        const events = factory[command](botId, requestConfig, customBot);

        expect(onTheWire(events)).toEqual([event(name)]);
      },
    );

    it.each([
      [
        "bot-deleted",
        REALTIME_BOT,
        gridConfig("1.1.0", "ETH/USD"),
        gridTrader("e1g5i9k3m7o1q5s9u3w7y1a5", "1.1.0"),
      ],
      [
        "bot-schedule-deleted",
        SCHEDULED_BOT,
        smaConfig("1.3.0", "MSFT", "30 15 * * 1-5"),
        smaCrossover("g3i7k1m5o9q3s7u1w5y9a3c7", "1.3.0"),
      ],
    ])("publishes %s", async (name, botId, storedConfig, versionInUse) => {
      customBotService.getGlobalSpecificVersion.mockResolvedValue(
        Ok(versionInUse),
      );

      const events = await factory.deleted(
        mockBot({ id: botId, config: { ...storedConfig, hasFrontend: false } }),
      );

      expect(events.success).toBe(true);
      expect(onTheWire(events.data!)).toEqual([event(name)]);
    });
  });

  it("publishes bot events only on subjects THE0_EVENTS holds", () => {
    const streams = loadContract<Record<string, string[]>>(
      "events/streams.json",
    );

    expect([
      ...Object.values(BOT_TOPICS),
      ...Object.values(SCHEDULED_BOT_TOPICS),
    ]).toEqual(streams.THE0_EVENTS);
  });
});
