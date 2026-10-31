import { Test, TestingModule } from "@nestjs/testing";
import { appError } from "@/common/app-error";
import { BotService } from "../bot.service";
import { BotRepository } from "../bot.repository";
import { Ok, Failure } from "@/common/result";
import { mockBot } from "@/test/mock-bot";
import { mockActor } from "@/test/mock-actor";

describe("BotService", () => {
  let service: BotService;
  let repository: BotRepository;
  const uid = "test-user-id";
  const actor = mockActor({ userId: uid });

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [BotService, BotRepository],
    }).compile();

    service = module.get<BotService>(BotService);
    repository = module.get<BotRepository>(BotRepository);

    // Reset all mocks
    jest.clearAllMocks();
  });

  describe("findAll", () => {
    it("should return all user bots", async () => {
      const bots = [
        mockBot({ id: "bot1", name: "Bot 1" }),
        mockBot({ id: "bot2", name: "Bot 2" }),
      ];

      jest.spyOn(repository, "findAll").mockResolvedValue({
        success: true,
        error: null,
        data: bots,
      });

      const result = await service.findAll(actor);

      expect(result.success).toBe(true);
      expect(result.data).toEqual(bots);
      expect(repository.findAll).toHaveBeenCalledWith(uid);
    });

    it("should handle repository errors", async () => {
      jest.spyOn(repository, "findAll").mockResolvedValue({
        success: false,
        error: appError("internal", "Database error"),
        data: null,
      });

      const result = await service.findAll(actor);

      expect(result.success).toBe(false);
      expect(result.error).toEqual(appError("internal", "Database error"));
    });
  });

  describe("findOne", () => {
    it("should return a specific bot", async () => {
      const bot = mockBot({ id: "test-id", name: "Test Bot" });

      jest.spyOn(repository, "findOne").mockResolvedValue({
        success: true,
        error: null,
        data: bot,
      });

      const result = await service.findOne(actor, "test-id");

      expect(result.success).toBe(true);
      expect(result.data).toEqual(bot);
      expect(repository.findOne).toHaveBeenCalledWith(uid, "test-id");
    });

    it("should handle bot not found", async () => {
      jest.spyOn(repository, "findOne").mockResolvedValue({
        success: false,
        error: appError("not_found", "Bot not found"),
        data: null,
      });

      const result = await service.findOne(actor, "nonexistent-id");

      expect(result.success).toBe(false);
      expect(result.error).toEqual(appError("not_found", "Bot not found"));
    });
  });

  describe("ownership follows the Actor", () => {
    const mcpActor = mockActor({
      userId: "mcp-user-id",
      via: "mcp",
      apiKeyId: "key-1",
    });

    it("reads bots as the Actor's user, whatever the channel", async () => {
      jest.spyOn(repository, "findAll").mockResolvedValue(Ok([]));
      jest
        .spyOn(repository, "findOne")
        .mockResolvedValue(Failure(appError("not_found", "missing")));

      await service.findAll(mcpActor);
      await service.findOne(mcpActor, "test-id");

      expect(repository.findAll).toHaveBeenCalledWith("mcp-user-id");
      expect(repository.findOne).toHaveBeenCalledWith("mcp-user-id", "test-id");
    });
  });
});
