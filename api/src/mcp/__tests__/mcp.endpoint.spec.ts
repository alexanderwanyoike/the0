import { INestApplication } from "@nestjs/common";
import { Test } from "@nestjs/testing";
import { PinoLogger } from "nestjs-pino";
import request from "supertest";
import { McpController } from "../mcp.controller";
import { McpService } from "../mcp.service";
import { MCP_TOOL_DEFINITIONS } from "../mcp.types";
import { ApiKeyService } from "@/api-key/api-key.service";
import { AuthService } from "@/auth/auth.service";
import { BotRepository } from "@/bot/bot.repository";
import { BotService } from "@/bot/bot.service";
import { BotValidator } from "@/bot/bot.validator";
import { BotQueryService } from "@/bot-query/bot-query.service";
import { BotStateService } from "@/bot-state/bot-state.service";
import { Failure, Ok } from "@/common/result";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { LogsService } from "@/logs/logs.service";
import { NatsService } from "@/nats/nats.service";
import { mockBot } from "@/test/mock-bot";
import { createMockLogger } from "@/test/mock-logger";
import { mockActor } from "@/test/mock-actor";

describe("POST /mcp", () => {
  let app: INestApplication;
  const apiKeyService = { validateApiKey: jest.fn() };
  const authService = { validateToken: jest.fn() };
  const botRepository = { findAll: jest.fn() };

  beforeAll(async () => {
    const moduleRef = await Test.createTestingModule({
      controllers: [McpController],
      providers: [
        McpService,
        BotService,
        { provide: ApiKeyService, useValue: apiKeyService },
        { provide: AuthService, useValue: authService },
        { provide: BotRepository, useValue: botRepository },
        { provide: BotValidator, useValue: {} },
        { provide: CustomBotService, useValue: {} },
        { provide: NatsService, useValue: {} },
        { provide: LogsService, useValue: {} },
        { provide: BotStateService, useValue: {} },
        { provide: BotQueryService, useValue: {} },
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
      Ok({ id: "key-1", userId: "user-1", name: "agent" }),
    );
    authService.validateToken.mockResolvedValue(
      Ok({ id: "user-1", username: "agent", email: "agent@example.com" }),
    );
  });

  function rpc(body: Record<string, unknown>, apiKey?: string) {
    return rpcWithHeaders(body, apiKey ? { "x-api-key": apiKey } : {});
  }

  function rpcWithHeaders(
    body: Record<string, unknown>,
    headers: Record<string, string>,
  ) {
    return request(app.getHttpServer())
      .post("/mcp")
      .set(headers)
      .send({ jsonrpc: "2.0", ...body });
  }

  const botList = {
    method: "tools/call",
    params: { name: "bot_list", arguments: {} },
  };

  const keyHeaders = [
    { header: "x-api-key", headers: { "x-api-key": "the0_valid" } },
    {
      header: "Authorization: ApiKey",
      headers: { authorization: "ApiKey the0_valid" },
    },
  ];
  const keyActor = mockActor({
    userId: "user-1",
    via: "mcp",
    apiKeyId: "key-1",
  });

  describe("completes the MCP handshake", () => {
    it("answers initialize with the protocol version and tool capability", async () => {
      const res = await rpc({ id: 1, method: "initialize", params: {} });

      expect(res.status).toBe(200);
      expect(res.body).toEqual({
        jsonrpc: "2.0",
        id: 1,
        result: {
          protocolVersion: "2024-11-05",
          capabilities: { tools: {} },
          serverInfo: { name: "the0-mcp", version: "1.0.0" },
        },
      });
    });
  });

  describe("answers the handshake without valid credentials", () => {
    it("answers initialize and tools/list for a key that does not validate", async () => {
      apiKeyService.validateApiKey.mockResolvedValue(
        Failure("API key not found or inactive"),
      );

      const initialize = await rpc(
        { id: 8, method: "initialize", params: {} },
        "the0_unknown",
      );
      const toolsList = await rpc(
        { id: 9, method: "tools/list" },
        "the0_unknown",
      );

      expect(initialize.status).toBe(200);
      expect(initialize.body.result.serverInfo).toEqual({
        name: "the0-mcp",
        version: "1.0.0",
      });
      expect(toolsList.status).toBe(200);
      expect(toolsList.body.result).toEqual({ tools: MCP_TOOL_DEFINITIONS });
    });
  });

  describe("lists the tools clients can call", () => {
    it("returns every tool definition", async () => {
      const res = await rpc({ id: 2, method: "tools/list" });

      expect(res.status).toBe(200);
      expect(res.body).toEqual({
        jsonrpc: "2.0",
        id: 2,
        result: { tools: MCP_TOOL_DEFINITIONS },
      });
    });
  });

  describe("runs a tool on behalf of the API key's owner", () => {
    it("lists the owner's bots", async () => {
      botRepository.findAll.mockResolvedValue(
        Ok([mockBot({ id: "bot-1", name: "Momentum", userId: "user-1" })]),
      );

      const res = await rpc(
        {
          id: 3,
          method: "tools/call",
          params: { name: "bot_list", arguments: {} },
        },
        "the0_valid",
      );

      expect(res.status).toBe(200);
      expect(apiKeyService.validateApiKey).toHaveBeenCalledWith("the0_valid");
      expect(botRepository.findAll).toHaveBeenCalledWith("user-1");
      expect(res.body.id).toBe(3);
      expect(res.body.result.isError).toBeUndefined();
      const bots = JSON.parse(res.body.result.content[0].text);
      expect(bots).toEqual([
        expect.objectContaining({ id: "bot-1", name: "Momentum" }),
      ]);
    });

    it.each(keyHeaders)(
      "accepts the key in $header and passes its mcp Actor through to BotService",
      async ({ headers }) => {
        botRepository.findAll.mockResolvedValue(Ok([]));
        const handleToolCall = jest.spyOn(
          McpService.prototype,
          "handleToolCall",
        );
        const findAll = jest.spyOn(BotService.prototype, "findAll");

        const res = await rpcWithHeaders({ id: 5, ...botList }, headers);

        expect(res.body.result.isError).toBeUndefined();
        expect(apiKeyService.validateApiKey).toHaveBeenCalledWith("the0_valid");
        expect(handleToolCall).toHaveBeenCalledWith("bot_list", {}, keyActor);
        expect(findAll).toHaveBeenCalledWith(keyActor);
        expect(botRepository.findAll).toHaveBeenCalledWith("user-1");
        handleToolCall.mockRestore();
        findAll.mockRestore();
      },
    );

    it("runs a tool for the user of a JWT", async () => {
      botRepository.findAll.mockResolvedValue(Ok([]));

      const res = await rpcWithHeaders(
        { id: 6, ...botList },
        { authorization: "Bearer the0_jwt" },
      );

      expect(res.body.result.isError).toBeUndefined();
      expect(authService.validateToken).toHaveBeenCalledWith("the0_jwt");
      expect(botRepository.findAll).toHaveBeenCalledWith("user-1");
    });

    it("refuses a tool call without credentials", async () => {
      const res = await rpc({ id: 7, ...botList });

      expect(res.status).toBe(200);
      expect(res.body.error).toEqual({
        code: -32001,
        message: "Authentication required. Provide x-api-key header.",
      });
      expect(botRepository.findAll).not.toHaveBeenCalled();
    });

    it("refuses a tool call from a caller whose key does not validate", async () => {
      apiKeyService.validateApiKey.mockResolvedValue(
        Failure("API key not found or inactive"),
      );

      const res = await rpc(
        {
          id: 4,
          method: "tools/call",
          params: { name: "bot_list", arguments: {} },
        },
        "the0_unknown",
      );

      expect(res.status).toBe(200);
      expect(res.body.error).toEqual({
        code: -32001,
        message: "Authentication required. Provide x-api-key header.",
      });
      expect(botRepository.findAll).not.toHaveBeenCalled();
    });
  });
});
