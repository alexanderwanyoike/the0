import { Scope } from "@nestjs/common";
import { Test, TestingModule } from "@nestjs/testing";
import { PinoLogger } from "nestjs-pino";
import { ApiKeyRepository } from "@/api-key/api-key.repository";
import { BotRepository } from "@/bot/bot.repository";
import { CustomBotRepository } from "@/custom-bot/custom-bot.repository";
import { createMockLogger } from "@/test/mock-logger";
import { useTestDatabase } from "@/test/test-database";

jest.mock(
  "@/database/connection",
  () => jest.requireActual("@/test/test-database").connection,
);

describe("repository logging", () => {
  useTestDatabase();

  it.each([
    {
      context: "ApiKeyRepository",
      failWrite: (module: TestingModule) =>
        module.get(ApiKeyRepository).createApiKey("missing-user", "example"),
    },
    {
      context: "BotRepository",
      failWrite: (module: TestingModule) =>
        module
          .get(BotRepository)
          .createWithEvents({ userId: "missing-user" }, []),
    },
    {
      context: "CustomBotRepository",
      failWrite: (module: TestingModule) =>
        module.get(CustomBotRepository).create({ userId: "missing-user" }),
    },
  ])(
    "sends $context failures through its injected logger",
    async ({ context, failWrite }) => {
      const loggers: ReturnType<typeof createMockLogger>[] = [];
      const module = await Test.createTestingModule({
        providers: [
          ApiKeyRepository,
          BotRepository,
          CustomBotRepository,
          {
            provide: PinoLogger,
            scope: Scope.TRANSIENT,
            useFactory: () => {
              const logger = createMockLogger();
              loggers.push(logger);
              return logger;
            },
          },
        ],
      }).compile();
      try {
        const result = await failWrite(module);
        expect(result.success).toBe(false);
        const logger = loggers.find((value) =>
          value.setContext.mock.calls.some(([name]) => name === context),
        );
        expect(logger).toBeDefined();
        expect(logger.error).toHaveBeenCalledWith(
          { err: expect.any(Error) },
          expect.stringContaining("Error creating"),
        );
        expect(logger.error).toHaveBeenCalledTimes(1);
        expect(
          loggers
            .filter((value) => value !== logger)
            .every((value) => value.error.mock.calls.length === 0),
        ).toBe(true);
      } finally {
        await module.close();
      }
    },
  );
});
