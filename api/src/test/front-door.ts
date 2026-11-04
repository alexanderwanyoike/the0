import { INestApplication, ValidationPipe } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { Test } from "@nestjs/testing";
import { PinoLogger } from "nestjs-pino";
import { ApiKeyService } from "@/api-key/api-key.service";
import { AuthService } from "@/auth/auth.service";
import { BotService } from "@/bot/bot.service";
import { appError, READ_FAILED } from "@/common/app-error";
import { Failure, Ok } from "@/common/result";
import { MINIO_CLIENT } from "@/minio";
import { mockBot } from "./mock-bot";
import { unsetConfig } from "./mock-config";
import { createMockLogger } from "./mock-logger";

/** The user every front door test calls as, and the one bot they own. */
export const OWNER = "user-1";
export const OWNED_BOT = "bot-1";
/** A bot whose ownership lookup fails in the database. */
export const UNREADABLE_BOT = "bot-unreadable";
export const AS_OWNER = { "x-api-key": "the0_owner" };

/**
 * A Nest app serving `controllers` as main.ts configures it, behind the real
 * AuthCombinedGuard, where OWNER owns OWNED_BOT and nothing else, and storage
 * is the given MinIO mock.
 */
export async function frontDoor(options: {
  controllers: unknown[];
  providers: unknown[];
  minio?: Record<string, jest.Mock>;
}): Promise<INestApplication> {
  const botService = {
    findOne: jest.fn(async (actor: { userId: string }, id: string) =>
      ownedBot(actor.userId, id),
    ),
    findOneByUserId: jest.fn(async (userId: string, id: string) =>
      ownedBot(userId, id),
    ),
  };
  const moduleRef = await Test.createTestingModule({
    controllers: options.controllers as never[],
    providers: [
      ...(options.providers as never[]),
      { provide: BotService, useValue: botService },
      { provide: MINIO_CLIENT, useValue: options.minio ?? {} },
      { provide: ConfigService, useValue: unsetConfig() },
      {
        provide: ApiKeyService,
        useValue: {
          validateApiKey: async () =>
            Ok({ id: "key-1", userId: OWNER, name: "cli" }),
        },
      },
      { provide: AuthService, useValue: { validateToken: jest.fn() } },
      { provide: PinoLogger, useValue: createMockLogger() },
    ],
  }).compile();

  const app = moduleRef.createNestApplication();
  app.useGlobalPipes(
    new ValidationPipe({
      whitelist: true,
      forbidNonWhitelisted: true,
      transform: true,
    }),
  );
  await app.init();
  return app;
}

const ownedBot = (userId: string, id: string) => {
  if (id === UNREADABLE_BOT) return Failure(READ_FAILED);
  return userId === OWNER && id === OWNED_BOT
    ? Ok(mockBot({ id, userId }))
    : Failure(appError("not_found", "Not found"));
};

export const errorBody = (statusCode: number, message: string) => ({
  statusCode,
  message,
  error: {
    400: "Bad Request",
    404: "Not Found",
    409: "Conflict",
    413: "Payload Too Large",
    422: "Unprocessable Entity",
    500: "Internal Server Error",
    503: "Service Unavailable",
    504: "Gateway Timeout",
  }[statusCode],
});
