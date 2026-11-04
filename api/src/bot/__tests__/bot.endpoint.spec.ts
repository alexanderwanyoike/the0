import { INestApplication } from "@nestjs/common";
import { Test } from "@nestjs/testing";
import { PinoLogger } from "nestjs-pino";
import request from "supertest";
import { BotController } from "../bot.controller";
import { BotRepository } from "../bot.repository";
import { BotService } from "../bot.service";
import { ApiKeyService } from "@/api-key/api-key.service";
import { AuthService } from "@/auth/auth.service";
import { appError } from "@/common/app-error";
import { Failure, Ok } from "@/common/result";
import { CommandBus } from "@/command-bus/command-bus";
import { mockBot } from "@/test/mock-bot";
import { createMockLogger } from "@/test/mock-logger";

describe("/bot routes", () => {
  let app: INestApplication;
  const apiKeyService = { validateApiKey: jest.fn() };
  const authService = { validateToken: jest.fn() };
  const ownersBot = mockBot({
    id: "bot-1",
    name: "Momentum",
    userId: "user-1",
  });
  const botRepository = {
    findAll: jest.fn(),
    findOne: jest.fn(async (userId: string, id: string) =>
      userId === ownersBot.userId && id === ownersBot.id
        ? Ok(ownersBot)
        : Failure(appError("not_found", "Not found")),
    ),
  };

  beforeAll(async () => {
    const moduleRef = await Test.createTestingModule({
      controllers: [BotController],
      providers: [
        BotService,
        { provide: BotRepository, useValue: botRepository },
        { provide: CommandBus, useValue: {} },
        { provide: ApiKeyService, useValue: apiKeyService },
        { provide: AuthService, useValue: authService },
        { provide: PinoLogger, useValue: createMockLogger() },
      ],
    }).compile();

    app = moduleRef.createNestApplication();
    await app.init();
  });

  afterAll(async () => {
    await app.close();
  });

  beforeEach(() => {
    jest.clearAllMocks();
    apiKeyService.validateApiKey.mockResolvedValue(
      Ok({ id: "key-1", userId: "user-1", name: "cli" }),
    );
    authService.validateToken.mockResolvedValue(
      Ok({ id: "user-1", username: "owner", email: "owner@example.com" }),
    );
    botRepository.findAll.mockResolvedValue(Ok([ownersBot]));
  });

  describe("act for the user each credential authenticates", () => {
    it.each([
      { credential: "x-api-key", headers: { "x-api-key": "the0_valid" } },
      {
        credential: "Authorization: ApiKey",
        headers: { authorization: "ApiKey the0_valid" },
      },
      {
        credential: "Authorization: Bearer",
        headers: { authorization: "Bearer the0_jwt" },
      },
    ])(
      "lists the bots of the user authenticated by $credential",
      async ({ headers }) => {
        const res = await request(app.getHttpServer()).get("/bot").set(headers);

        expect(res.status).toBe(200);
        expect(res.body).toEqual([
          expect.objectContaining({ id: "bot-1", name: "Momentum" }),
        ]);
        expect(botRepository.findAll).toHaveBeenCalledWith("user-1");
      },
    );

    it("uses a valid x-api-key even when a stale Authorization header is sent too", async () => {
      authService.validateToken.mockResolvedValue(Failure("Invalid token"));

      const res = await request(app.getHttpServer())
        .get("/bot")
        .set({ authorization: "Bearer stale", "x-api-key": "the0_valid" });

      expect(res.status).toBe(200);
      expect(authService.validateToken).not.toHaveBeenCalled();
      expect(botRepository.findAll).toHaveBeenCalledWith("user-1");
    });

    it("treats another user's bot as not found", async () => {
      apiKeyService.validateApiKey.mockResolvedValue(
        Ok({ id: "key-2", userId: "user-2", name: "cli" }),
      );

      const res = await request(app.getHttpServer())
        .get("/bot/bot-1")
        .set("x-api-key", "the0_other");

      expect(res.status).toBe(404);
      expect(botRepository.findOne).toHaveBeenCalledWith("user-2", "bot-1");
    });
  });

  describe("refuse callers without valid credentials", () => {
    it("answers a request without credentials with 401", async () => {
      const res = await request(app.getHttpServer()).get("/bot");

      expect(res.status).toBe(401);
      expect(botRepository.findAll).not.toHaveBeenCalled();
    });

    it("answers a key that does not validate with 401", async () => {
      apiKeyService.validateApiKey.mockResolvedValue(
        Failure("API key not found or inactive"),
      );

      const res = await request(app.getHttpServer())
        .get("/bot")
        .set("x-api-key", "the0_unknown");

      expect(res.status).toBe(401);
      expect(botRepository.findAll).not.toHaveBeenCalled();
    });
  });
});
