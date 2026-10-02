import { Test, TestingModule } from "@nestjs/testing";
import { McpController } from "../mcp.controller";
import { McpService } from "../mcp.service";
import { ApiKeyService } from "@/api-key/api-key.service";
import { Ok, Failure } from "@/common/result";
import { createMockLogger } from "@/test/mock-logger";
import { PinoLogger } from "nestjs-pino";

describe("McpController", () => {
  let controller: McpController;
  let mcpService: jest.Mocked<McpService>;
  let apiKeyService: jest.Mocked<ApiKeyService>;

  const mockApiKey = {
    id: "key-123",
    userId: "user-123",
    name: "Test Key",
    key: "the0_testkey",
    isActive: true,
    createdAt: new Date(),
    updatedAt: new Date(),
  };

  beforeEach(async () => {
    const mockMcpService = {
      handleToolCall: jest.fn(),
    };

    const mockApiKeyService = {
      validateApiKey: jest.fn(),
    };

    const module: TestingModule = await Test.createTestingModule({
      controllers: [McpController],
      providers: [
        {
          provide: McpService,
          useValue: mockMcpService,
        },
        {
          provide: ApiKeyService,
          useValue: mockApiKeyService,
        },
        {
          provide: PinoLogger,
          useValue: createMockLogger(),
        },
      ],
    }).compile();

    controller = module.get<McpController>(McpController);
    mcpService = module.get(McpService);
    apiKeyService = module.get(ApiKeyService);
  });

  describe("handleRpc", () => {
    describe("initialize", () => {
      it("should return server info and capabilities", async () => {
        const request = {
          jsonrpc: "2.0" as const,
          id: 1,
          method: "initialize",
        };

        const result = await controller.handleRpc(request);

        expect(result).toEqual({
          jsonrpc: "2.0",
          id: 1,
          result: {
            protocolVersion: "2024-11-05",
            capabilities: {
              tools: {},
            },
            serverInfo: {
              name: "the0-mcp",
              version: "1.0.0",
            },
          },
        });
      });
    });

    describe("tools/list", () => {
      it("should return list of available tools", async () => {
        const request = {
          jsonrpc: "2.0" as const,
          id: 2,
          method: "tools/list",
        };

        const result = await controller.handleRpc(request);

        expect(result.jsonrpc).toBe("2.0");
        expect(result.id).toBe(2);
        expect(result.result).toHaveProperty("tools");
        expect(Array.isArray((result.result as any).tools)).toBe(true);

        const tools = (result.result as any).tools;
        const toolNames = tools.map((t: any) => t.name);

        // Verify all expected tools are present
        expect(toolNames).toContain("auth_status");
        expect(toolNames).toContain("bot_list");
        expect(toolNames).toContain("bot_get");
        expect(toolNames).toContain("bot_deploy");
        expect(toolNames).toContain("bot_update");
        expect(toolNames).toContain("bot_delete");
        expect(toolNames).toContain("logs_get");
        expect(toolNames).toContain("logs_summary");
        expect(toolNames).toContain("bot_state_list");
        expect(toolNames).toContain("bot_state_get");
        expect(toolNames).toContain("bot_query");
        expect(toolNames).toContain("custom_bot_list");
        expect(toolNames).toContain("custom_bot_get");
        expect(toolNames).toContain("custom_bot_schema");
      });

      it("should have proper inputSchema for each tool", async () => {
        const request = {
          jsonrpc: "2.0" as const,
          id: 1,
          method: "tools/list",
        };

        const result = await controller.handleRpc(request);
        const tools = (result.result as any).tools;

        for (const tool of tools) {
          expect(tool).toHaveProperty("name");
          expect(tool).toHaveProperty("description");
          expect(tool).toHaveProperty("inputSchema");
          expect(tool.inputSchema).toHaveProperty("type", "object");
          expect(tool.inputSchema).toHaveProperty("properties");
          expect(tool.inputSchema).toHaveProperty("required");
        }
      });

      it("should advertise the required arguments each tool handler reads", async () => {
        const result = await controller.handleRpc({
          jsonrpc: "2.0" as const,
          id: 1,
          method: "tools/list",
        });
        const tools = (result.result as any).tools;

        const requiredByTool = Object.fromEntries(
          tools.map((t: any) => [t.name, t.inputSchema.required]),
        );
        expect(requiredByTool).toEqual({
          auth_status: [],
          bot_list: [],
          bot_get: ["bot_id"],
          bot_deploy: ["config"],
          bot_update: ["bot_id", "config"],
          bot_delete: ["bot_id"],
          logs_get: ["bot_id"],
          logs_summary: ["bot_id"],
          bot_state_list: ["bot_id"],
          bot_state_get: ["bot_id", "key"],
          bot_query: ["bot_id", "query_path"],
          custom_bot_list: [],
          custom_bot_get: ["name"],
          custom_bot_schema: ["name"],
        });
      });

      it("should take the bot_deploy name inside config, where the handler reads it", async () => {
        const result = await controller.handleRpc({
          jsonrpc: "2.0" as const,
          id: 1,
          method: "tools/list",
        });
        const deploy = (result.result as any).tools.find(
          (t: any) => t.name === "bot_deploy",
        );

        expect(Object.keys(deploy.inputSchema.properties)).toEqual(["config"]);
        expect(deploy.inputSchema.properties.config.description).toContain(
          "name",
        );
      });
    });

    describe("tools/call", () => {
      it("should require authentication", async () => {
        const request = {
          jsonrpc: "2.0" as const,
          id: 3,
          method: "tools/call",
          params: {
            name: "bot_list",
            arguments: {},
          },
        };

        const result = await controller.handleRpc(request);

        expect(result).toEqual({
          jsonrpc: "2.0",
          id: 3,
          error: {
            code: -32001,
            message: "Authentication required. Provide x-api-key header.",
          },
        });
      });

      it("should require tool name", async () => {
        apiKeyService.validateApiKey.mockResolvedValue(Ok(mockApiKey));

        const request = {
          jsonrpc: "2.0" as const,
          id: 4,
          method: "tools/call",
          params: {
            arguments: {},
          },
        };

        const result = await controller.handleRpc(request, "valid-api-key");

        expect(result).toEqual({
          jsonrpc: "2.0",
          id: 4,
          error: {
            code: -32602,
            message: "Missing tool name",
          },
        });
      });

      it("should call McpService.handleToolCall with valid request", async () => {
        apiKeyService.validateApiKey.mockResolvedValue(Ok(mockApiKey));
        mcpService.handleToolCall.mockResolvedValue({
          content: [{ type: "text", text: '{"bots": []}' }],
        });

        const request = {
          jsonrpc: "2.0" as const,
          id: 5,
          method: "tools/call",
          params: {
            name: "bot_list",
            arguments: {},
          },
        };

        const result = await controller.handleRpc(request, "valid-api-key");

        expect(apiKeyService.validateApiKey).toHaveBeenCalledWith(
          "valid-api-key",
        );
        expect(mcpService.handleToolCall).toHaveBeenCalledWith(
          "bot_list",
          {},
          "user-123",
        );
        expect(result).toEqual({
          jsonrpc: "2.0",
          id: 5,
          result: {
            content: [{ type: "text", text: '{"bots": []}' }],
          },
        });
      });

      it("should not authenticate with invalid API key", async () => {
        apiKeyService.validateApiKey.mockResolvedValue(
          Failure("Invalid API key"),
        );

        const request = {
          jsonrpc: "2.0" as const,
          id: 6,
          method: "tools/call",
          params: {
            name: "bot_list",
            arguments: {},
          },
        };

        const result = await controller.handleRpc(request, "invalid-api-key");

        expect(result).toEqual({
          jsonrpc: "2.0",
          id: 6,
          error: {
            code: -32001,
            message: "Authentication required. Provide x-api-key header.",
          },
        });
      });
    });

    describe("ping", () => {
      it("should respond to ping", async () => {
        const request = {
          jsonrpc: "2.0" as const,
          id: 7,
          method: "ping",
        };

        const result = await controller.handleRpc(request);

        expect(result).toEqual({
          jsonrpc: "2.0",
          id: 7,
          result: {},
        });
      });
    });

    describe("unknown method", () => {
      it("should return error for unknown method", async () => {
        const request = {
          jsonrpc: "2.0" as const,
          id: 8,
          method: "unknown/method",
        };

        const result = await controller.handleRpc(request);

        expect(result).toEqual({
          jsonrpc: "2.0",
          id: 8,
          error: {
            code: -32601,
            message: "Method not found: unknown/method",
          },
        });
      });
    });

    describe("error handling", () => {
      it("should handle service errors gracefully", async () => {
        apiKeyService.validateApiKey.mockResolvedValue(Ok(mockApiKey));
        mcpService.handleToolCall.mockRejectedValue(new Error("Service error"));

        const request = {
          jsonrpc: "2.0" as const,
          id: 9,
          method: "tools/call",
          params: {
            name: "bot_list",
            arguments: {},
          },
        };

        const result = await controller.handleRpc(request, "valid-api-key");

        expect(result).toEqual({
          jsonrpc: "2.0",
          id: 9,
          error: {
            code: -32603,
            message: "Service error",
          },
        });
      });
    });
  });
});
