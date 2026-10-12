import { CustomBotService } from "../custom-bot.service";
import { CustomBotRepository } from "../custom-bot.repository";
import { StorageService } from "../storage.service";
import { Ok } from "@/common/result";
import { customBotsTable } from "@/database/schema/custom-bots";
import { botsTable } from "@/database/schema/bots";
import { mockBotConfig } from "@/test/mock-bot";
import { createMockLogger } from "@/test/mock-logger";
import { TEST_OWNER_ID, useTestDatabase } from "@/test/test-database";

jest.mock(
  "@/database/connection",
  () => jest.requireActual("@/test/test-database").connection,
);

const OWNER = TEST_OWNER_ID;
const NAME = "sma-bot";

describe("CustomBotService deleting versions", () => {
  const db = useTestDatabase();
  let repository: CustomBotRepository;
  let storage: { listObjects: jest.Mock; deleteFile: jest.Mock };
  let service: CustomBotService;

  const versionIds = async () =>
    (await db().select({ id: customBotsTable.id }).from(customBotsTable))
      .map(({ id }) => id)
      .sort();
  const botIds = async () =>
    (await db().select({ id: botsTable.id }).from(botsTable)).map(
      ({ id }) => id,
    );

  const storeVersion = (id: string, version: string) =>
    db()
      .insert(customBotsTable)
      .values({
        id,
        userId: OWNER,
        name: NAME,
        version,
        config: {} as never,
        filePath: `${OWNER}/${NAME}/${version}`,
      });

  const deployBot = (id: string, customBotId: string) =>
    db()
      .insert(botsTable)
      .values({
        id,
        userId: OWNER,
        name: "SMA",
        config: mockBotConfig({ type: `scheduled/${NAME}` }),
        topic: "the0-scheduled-custom-bot",
        customBotId,
      });

  beforeEach(async () => {
    repository = new CustomBotRepository();
    storage = {
      listObjects: jest.fn().mockResolvedValue(Ok([])),
      deleteFile: jest.fn().mockResolvedValue(Ok(undefined)),
    };
    service = new CustomBotService(
      repository,
      storage as unknown as StorageService,
      createMockLogger() as never,
    );
    await storeVersion("cb-1", "1.0.0");
    await storeVersion("cb-2", "1.1.0");
  });

  describe("one version", () => {
    it("deletes a version no bot uses and cleans up its files", async () => {
      await deployBot("bot-1", "cb-1");

      const result = await service.deleteVersion(OWNER, NAME, "1.1.0");

      expect(result.success).toBe(true);
      expect(await versionIds()).toEqual(["cb-1"]);
      expect(await botIds()).toEqual(["bot-1"]);
      expect(storage.listObjects).toHaveBeenCalledWith(
        `${OWNER}/${NAME}/1.1.0`,
      );
    });

    it("keeps a version and its bot when the bot is deployed after the instance check", async () => {
      const count = repository.countInstancesByCustomBotId.bind(repository);
      jest
        .spyOn(repository, "countInstancesByCustomBotId")
        .mockImplementation(async (customBotId) => {
          const counted = await count(customBotId);
          await deployBot("bot-1", "cb-1");
          return counted;
        });

      const result = await service.deleteVersion(OWNER, NAME, "1.0.0");

      expect(await botIds()).toEqual(["bot-1"]);
      expect(await versionIds()).toEqual(["cb-1", "cb-2"]);
      expect(result.success).toBe(false);
      expect(result.error).toBe(
        "Cannot delete version 1.0.0: active instance(s) reference it",
      );
      expect(storage.listObjects).not.toHaveBeenCalled();
    });
  });

  describe("every version", () => {
    it("deletes every version when no bot uses them and cleans up their files", async () => {
      const result = await service.deleteAllVersions(OWNER, NAME);

      expect(result.success).toBe(true);
      expect(await versionIds()).toEqual([]);
      expect(storage.listObjects).toHaveBeenCalledWith(`${OWNER}/${NAME}/`);
    });

    it("keeps every version and the bot when the bot is deployed after the instance check", async () => {
      const count = repository.countInstancesByCustomBotIds.bind(repository);
      jest
        .spyOn(repository, "countInstancesByCustomBotIds")
        .mockImplementation(async (customBotIds) => {
          const counted = await count(customBotIds);
          await deployBot("bot-1", "cb-1");
          return counted;
        });

      const result = await service.deleteAllVersions(OWNER, NAME);

      expect(await botIds()).toEqual(["bot-1"]);
      expect(await versionIds()).toEqual(["cb-1", "cb-2"]);
      expect(result.success).toBe(false);
      expect(result.error).toBe(
        "Cannot delete: versions with active instance(s)",
      );
      expect(storage.listObjects).not.toHaveBeenCalled();
    });
  });
});
