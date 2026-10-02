import { INestApplication } from "@nestjs/common";
import { Test } from "@nestjs/testing";
import { PinoLogger } from "nestjs-pino";
import request from "supertest";
import { McpController } from "../mcp.controller";
import { McpService } from "../mcp.service";
import { MCP_TOOL_DEFINITIONS } from "../mcp.types";
import { ApiKeyService } from "@/api-key/api-key.service";
import { BotRepository } from "@/bot/bot.repository";
import { Failure, Ok } from "@/common/result";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { LogsService } from "@/logs/logs.service";
import { mockBot } from "@/test/mock-bot";
import { createMockLogger } from "@/test/mock-logger";

describe("POST /mcp", () => {
  let app: INestApplication;
  const apiKeyService = { validateApiKey: jest.fn() };
  const botRepository = { findAll: jest.fn() };

  beforeAll(async () => {
    const moduleRef = await Test.createTestingModule({
      controllers: [McpController],
      providers: [
        McpService,
        { provide: ApiKeyService, useValue: apiKeyService },
        { provide: BotRepository, useValue: botRepository },
        { provide: CustomBotService, useValue: {} },
        { provide: LogsService, useValue: {} },
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
  });

  function rpc(body: Record<string, unknown>, apiKey?: string) {
    const req = request(app.getHttpServer()).post("/mcp");
    return (apiKey ? req.set("x-api-key", apiKey) : req).send({
      jsonrpc: "2.0",
      ...body,
    });
  }

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
