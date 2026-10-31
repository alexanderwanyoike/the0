import { Readable } from "stream";
import { PinoLogger } from "nestjs-pino";
import * as Minio from "minio";
import { BotService } from "@/bot/bot.service";
import { Ok } from "@/common/result";
import { LogsService } from "@/logs/logs.service";
import { unsetConfig } from "./mock-config";
import { createMockLogger } from "./mock-logger";

/**
 * A LogsService whose storage holds `file` at every key, for any user, and
 * the storage mock, to see which keys it read.
 */
export function logsServiceServing(file: string) {
  const minio = {
    statObject: jest.fn().mockResolvedValue({ size: Buffer.byteLength(file) }),
    getObject: jest.fn(async () => Readable.from([Buffer.from(file)])),
    getPartialObject: jest.fn(),
  };
  const service = new LogsService(
    minio as unknown as Minio.Client,
    unsetConfig(),
    {
      findOneByUserId: jest.fn().mockResolvedValue(Ok({})),
    } as unknown as BotService,
    createMockLogger() as unknown as PinoLogger,
  );
  return { service, minio };
}
