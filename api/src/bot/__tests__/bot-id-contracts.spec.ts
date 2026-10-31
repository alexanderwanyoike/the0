import { createId } from "@paralleldrive/cuid2";
import { appError } from "@/common/app-error";
import { unsetConfig } from "@/test/mock-config";
import { PinoLogger } from "nestjs-pino";
import * as Minio from "minio";
import { BotService } from "@/bot/bot.service";
import { BotStateService } from "@/bot-state/bot-state.service";
import { Failure } from "@/common/result";
import { isValidBotId } from "@/logs/bot-id";
import { loadContract } from "@/test/contracts";
import { createMockLogger } from "@/test/mock-logger";
import { mockActor } from "@/test/mock-actor";

const { valid, invalid } = loadContract<{
  valid: string[];
  invalid: string[];
}>("bot-ids.json");

describe("bot ID contracts", () => {
  it("issues bot IDs the contract counts as valid", () => {
    for (let i = 0; i < 100; i++) {
      expect(isValidBotId(createId())).toBe(true);
    }
  });

  describe("read logs only for valid bot IDs", () => {
    it.each(valid)("accepts %j", (id) => {
      expect(isValidBotId(id)).toBe(true);
    });

    it.each(invalid)("rejects %j", (id) => {
      expect(isValidBotId(id)).toBe(false);
    });
  });

  describe("read state only for valid bot IDs", () => {
    let botService: { findOne: jest.Mock };
    let service: BotStateService;

    beforeEach(() => {
      botService = {
        findOne: jest
          .fn()
          .mockResolvedValue(Failure(appError("internal", "none"))),
      };
      service = new BotStateService(
        {} as Minio.Client,
        unsetConfig(),
        botService as unknown as BotService,
        createMockLogger() as unknown as PinoLogger,
      );
    });

    it.each(valid)("looks %j up", async (id) => {
      const result = await service.listKeys(mockActor(), id);

      expect(result.error?.code).toBe("not_found");
      expect(botService.findOne).toHaveBeenCalledWith(expect.anything(), id);
    });

    it.each(invalid)("refuses %j before any lookup", async (id) => {
      const result = await service.listKeys(mockActor(), id);

      expect(result.error).toEqual({
        code: "internal",
        message: "Invalid bot ID format",
      });
      expect(botService.findOne).not.toHaveBeenCalled();
    });
  });
});
