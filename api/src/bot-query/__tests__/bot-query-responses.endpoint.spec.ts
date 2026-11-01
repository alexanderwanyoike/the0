import { INestApplication } from "@nestjs/common";
import request from "supertest";
import { BotQueryController } from "../bot-query.controller";
import { BotQueryService } from "../bot-query.service";
import {
  AS_OWNER,
  errorBody,
  frontDoor,
  OWNED_BOT,
  UNREADABLE_BOT,
} from "@/test/front-door";

describe("/query responses", () => {
  let app: INestApplication;
  let runtime: jest.SpyInstance;

  beforeAll(async () => {
    app = await frontDoor({
      controllers: [BotQueryController],
      providers: [BotQueryService],
    });
  });

  afterAll(async () => {
    await app.close();
  });

  beforeEach(() => {
    runtime = jest.spyOn(global, "fetch");
  });

  afterEach(() => {
    runtime.mockRestore();
  });

  const query = (botId = OWNED_BOT) =>
    request(app.getHttpServer())
      .post(`/query/${botId}`)
      .set(AS_OWNER)
      .send({ query_path: "/today" });
  const runtimeAnswers = (status: number, body: string) =>
    runtime.mockResolvedValue(new Response(body, { status }));
  const runtimeThrows = (error: unknown) => runtime.mockRejectedValue(error);
  const expectAnswer = (
    res: request.Response,
    status: number,
    message: string,
  ) =>
    expect({ status: res.status, body: res.body }).toEqual({
      status,
      body: errorBody(status, message),
    });

  it("answers 201 with the bot's result", async () => {
    runtimeAnswers(200, JSON.stringify({ status: "ok", data: { pnl: 3 } }));

    const res = await query();

    expect(res.status).toBe(201);
    expect(res.body).toEqual({
      success: true,
      data: expect.objectContaining({ status: "ok", data: { pnl: 3 } }),
      message: "Query executed successfully",
    });
  });

  it("answers a bot the user cannot see with 404", async () => {
    const res = await query("someone-elses");

    expectAnswer(res, 404, "Bot not found or access denied");
    expect(runtime).not.toHaveBeenCalled();
  });

  it("answers a failed ownership lookup with 500", async () => {
    expectAnswer(
      await query(UNREADABLE_BOT),
      500,
      "Failed to read from the database",
    );
    expect(runtime).not.toHaveBeenCalled();
  });

  it("answers a bot the runtime does not hold with 404", async () => {
    runtimeAnswers(404, "bot not running");

    expectAnswer(await query(), 404, "Bot not found in runtime");
  });

  it("answers a bot that defines no queries with 422", async () => {
    runtimeAnswers(422, "no query entrypoint");

    expectAnswer(
      await query(),
      422,
      "This bot defines no queries. Add a query entrypoint (entrypoints.query in bot-config.yaml) to answer them.",
    );
  });

  it("answers the bot's error result with 400 and the runtime's answer", async () => {
    runtimeAnswers(500, '{"status":"error","error":"unknown symbol"}');

    expectAnswer(
      await query(),
      400,
      'Query failed: {"status":"error","error":"unknown symbol"}',
    );
  });

  it("answers an unreachable runtime with 503", async () => {
    runtimeThrows(
      Object.assign(new TypeError("fetch failed"), {
        cause: { code: "ECONNREFUSED" },
      }),
    );

    expectAnswer(
      await query(),
      503,
      "Bot runtime is not available. Ensure the bot-runner service is running.",
    );
  });

  it("answers a query that times out with 504", async () => {
    runtimeThrows(Object.assign(new Error("aborted"), { name: "AbortError" }));

    expectAnswer(await query(), 504, "Query timed out after 30 seconds");
  });

  it("answers any other runtime failure with 500", async () => {
    runtimeThrows(new Error("socket hang up"));

    expectAnswer(await query(), 500, "Query failed: socket hang up");
  });

  it("refuses a request without a query path through the DTO", async () => {
    const res = await request(app.getHttpServer())
      .post(`/query/${OWNED_BOT}`)
      .set(AS_OWNER)
      .send({});

    expect(res.status).toBe(400);
    expect(res.body.message).toEqual(["query_path must be a string"]);
  });
});
