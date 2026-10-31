import { ConfigService } from "@nestjs/config";
import { PinoLogger } from "nestjs-pino";
import { BotService } from "@/bot/bot.service";
import { Ok } from "@/common/result";
import { loadContract } from "@/test/contracts";
import { createMockLogger } from "@/test/mock-logger";
import { mockActor } from "@/test/mock-actor";
import { mockBot } from "@/test/mock-bot";
import { BotQueryService, QueryResult } from "../bot-query.service";

interface QueryContract {
  port: number;
  path: string;
  request: {
    bot_id: string;
    query_path: string;
    params: Record<string, unknown>;
    timeout_sec: number;
  };
  responses: {
    case: string;
    status: number;
    body: Record<string, unknown>;
    api: {
      result?: QueryResult;
      error?: { code: string; message?: string; messageContains?: string };
    };
  }[];
}

const contract = loadContract<QueryContract>("query.json");

describe("query contracts", () => {
  let fetch: jest.SpyInstance;
  let service: BotQueryService;

  beforeEach(() => {
    fetch = jest.spyOn(global, "fetch");
    service = new BotQueryService(
      { get: (): undefined => undefined } as unknown as ConfigService,
      {
        findOne: jest.fn().mockResolvedValue(Ok(mockBot())),
      } as unknown as BotService,
      createMockLogger() as unknown as PinoLogger,
    );
  });

  afterEach(() => fetch.mockRestore());

  const ask = () =>
    service.executeQuery(mockActor(), contract.request.bot_id, {
      queryPath: contract.request.query_path,
      params: contract.request.params,
      timeoutSec: contract.request.timeout_sec,
    });

  it("asks the runtime's query server on its default port", async () => {
    fetch.mockResolvedValue(new Response("{}", { status: 200 }));

    await ask();

    const [url, init] = fetch.mock.calls[0];
    expect(url).toBe(`http://localhost:${contract.port}${contract.path}`);
    expect(init.method).toBe("POST");
    expect(JSON.parse(init.body)).toEqual(contract.request);
  });

  describe("report each answer of the query server", () => {
    it.each(contract.responses)("reports $case", async (response) => {
      fetch.mockResolvedValue(
        new Response(JSON.stringify(response.body), {
          status: response.status,
          headers: { "Content-Type": "application/json" },
        }),
      );

      const result = await ask();

      if (response.api.result) {
        expect(result).toEqual(Ok(response.api.result));
        return;
      }
      const expected = response.api.error!;
      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(expected.code);
      if (expected.message !== undefined) {
        expect(result.error?.message).toBe(expected.message);
      }
      if (expected.messageContains !== undefined) {
        expect(result.error?.message).toContain(expected.messageContains);
      }
    });
  });
});
