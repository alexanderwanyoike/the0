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

interface EventContract {
  subject: string;
  payload: {
    id: string;
    config: BotConfig & { customBotId?: string };
    custom?: {
      config: CustomBot["config"];
      createdAt: string;
      updatedAt: string;
      filePath: string;
      version: string;
    };
  };
}

const event = (name: string) =>
  loadContract<EventContract>(`events/${name}.json`);

/** The Catalogue row the contract's payload was built from. */
function customBotOf({ payload }: EventContract): CustomBot {
  const custom = payload.custom!;
  return {
    id: payload.config.customBotId!,
    name: custom.config.name,
    version: custom.version,
    config: custom.config,
    filePath: custom.filePath,
    userId: "uploader-1",
    status: "active",
    createdAt: new Date(custom.createdAt),
    updatedAt: new Date(custom.updatedAt),
  };
}

/** The Bot's config as the request carried it, before the event adds to it. */
function requestConfigOf({ payload }: EventContract): BotConfig {
  const { customBotId: _customBotId, ...config } = payload.config;
  return config;
}

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
      ["bot-created", "created"],
      ["bot-schedule-created", "created"],
      ["bot-updated", "updated"],
      ["bot-schedule-updated", "updated"],
    ] as const)("publishes %s", (name, command) => {
      const contract = event(name);

      const events = factory[command](
        contract.payload.id,
        requestConfigOf(contract),
        customBotOf(contract),
      );

      expect(onTheWire(events)).toEqual([contract]);
    });

    it.each(["bot-deleted", "bot-schedule-deleted"])(
      "publishes %s",
      async (name) => {
        const contract = event(name);
        const created = event(name.replace("deleted", "updated"));
        customBotService.getGlobalSpecificVersion.mockResolvedValue(
          Ok(customBotOf(created)),
        );

        const events = await factory.deleted(
          mockBot({ id: contract.payload.id, config: contract.payload.config }),
        );

        expect(events.success).toBe(true);
        expect(onTheWire(events.data!)).toEqual([contract]);
      },
    );
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
