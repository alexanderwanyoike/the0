import { Readable } from "stream";
import { ConfigService } from "@nestjs/config";
import { PinoLogger } from "nestjs-pino";
import * as Minio from "minio";
import { BotService } from "@/bot/bot.service";
import {
  BotStateService,
  stateObjectName,
} from "@/bot-state/bot-state.service";
import { Ok } from "@/common/result";
import { loadContract } from "@/test/contracts";
import { createMockLogger } from "@/test/mock-logger";
import { mockActor } from "@/test/mock-actor";
import { utcDay } from "../log-entry";
import { LogsService } from "../logs.service";

const keys = loadContract<{
  logs: { botId: string; at: string; date: string; key: string }[];
  state: { botId: string; key: string }[];
}>("storage-keys.json");

const config = { get: (): undefined => undefined } as unknown as ConfigService;
const logger = createMockLogger() as unknown as PinoLogger;

describe("storage key contracts", () => {
  it.each(keys.logs)(
    "reads $botId's logs for $date from $key",
    async ({ botId, date, key }) => {
      const line = '{"message":"hello"}\n';
      const minio = {
        statObject: jest
          .fn()
          .mockResolvedValue({ size: Buffer.byteLength(line) }),
        getObject: jest.fn(async () => Readable.from([Buffer.from(line)])),
        getPartialObject: jest.fn(),
      };
      const service = new LogsService(
        minio as unknown as Minio.Client,
        config,
        {
          findOneByUserId: jest.fn().mockResolvedValue(Ok({})),
        } as unknown as BotService,
        logger,
      );

      const page = await service.getLogs(
        botId,
        { date, limit: 10, offset: 0, sort: "asc" },
        "user-1",
      );

      expect(page.success).toBe(true);
      expect(minio.getObject).toHaveBeenCalledWith(expect.any(String), key);
    },
  );

  it.each(keys.logs)(
    "files a line written at $at under $date, as the runtime does",
    ({ at, date }) => {
      expect(utcDay(new Date(at))).toBe(date);
    },
  );

  it.each(keys.state)(
    "clears $botId's state at $key",
    async ({ botId, key }) => {
      const minio = {
        statObject: jest.fn().mockResolvedValue({ size: 1 }),
        removeObject: jest.fn().mockResolvedValue(undefined),
      };
      const service = new BotStateService(
        minio as unknown as Minio.Client,
        config,
        {
          findOne: jest.fn().mockResolvedValue(Ok({})),
        } as unknown as BotService,
        logger,
      );

      const cleared = await service.clearState(mockActor(), botId);

      expect(cleared.success).toBe(true);
      expect(minio.statObject).toHaveBeenCalledWith(expect.any(String), key);
      expect(minio.removeObject).toHaveBeenCalledWith(expect.any(String), key);
    },
  );

  it.each(keys.state)(
    "downloads $botId's state from $key",
    async ({ botId, key }) => {
      const minio = {
        statObject: jest.fn().mockResolvedValue({ size: 1, etag: "e" }),
        fGetObject: jest.fn().mockRejectedValue(new Error("stop here")),
      };
      const service = new BotStateService(
        minio as unknown as Minio.Client,
        config,
        {
          findOne: jest.fn().mockResolvedValue(Ok({})),
        } as unknown as BotService,
        logger,
      );

      await service.listKeys(mockActor(), botId);

      expect(minio.fGetObject).toHaveBeenCalledWith(
        expect.any(String),
        key,
        expect.any(String),
      );
    },
  );

  // Reads, writes and clears all name the object through this one function.
  it.each(keys.state)("names $botId's state object $key", ({ botId, key }) => {
    expect(stateObjectName(botId)).toBe(key);
  });
});
