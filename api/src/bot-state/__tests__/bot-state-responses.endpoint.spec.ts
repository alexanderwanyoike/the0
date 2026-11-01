import { INestApplication } from "@nestjs/common";
import request from "supertest";
import { BotStateController } from "../bot-state.controller";
import { BotStateService } from "../bot-state.service";
import {
  AS_OWNER,
  errorBody,
  frontDoor,
  OWNED_BOT,
  UNREADABLE_BOT,
} from "@/test/front-door";

const storageMissing = () =>
  Object.assign(new Error("Not Found"), { code: "NotFound" });
const storageDown = () => new Error("storage down");

describe("/bots/:botId/state responses", () => {
  let app: INestApplication;
  const minio = {
    statObject: jest.fn(),
    fGetObject: jest.fn(),
    removeObject: jest.fn(),
  };

  beforeAll(async () => {
    app = await frontDoor({
      controllers: [BotStateController],
      providers: [BotStateService],
      minio,
    });
  });

  afterAll(async () => {
    await app.close();
  });

  beforeEach(() => {
    minio.statObject.mockRejectedValue(storageMissing());
    minio.removeObject.mockResolvedValue(undefined);
  });

  const api = () => request(app.getHttpServer());
  const expectAnswer = (
    res: request.Response,
    status: number,
    message: string,
  ) =>
    expect({ status: res.status, body: res.body }).toEqual({
      status,
      body: errorBody(status, message),
    });

  describe("succeed on a bot without state", () => {
    it("lists no keys", async () => {
      const res = await api().get(`/bots/${OWNED_BOT}/state`).set(AS_OWNER);

      expect(res.status).toBe(200);
      expect(res.body).toEqual({
        success: true,
        data: [],
        message: "State keys retrieved successfully",
      });
    });

    it("reports a missing key as not deleted", async () => {
      const res = await api()
        .delete(`/bots/${OWNED_BOT}/state/positions`)
        .set(AS_OWNER);

      expect(res.status).toBe(200);
      expect(res.body).toEqual({
        success: true,
        data: { deleted: false },
        message: "State key not found",
      });
    });
  });

  describe("answer a bot the user cannot see", () => {
    it.each([
      { request: "a list", method: "get", path: "/state" },
      { request: "a key delete", method: "delete", path: "/state/positions" },
      { request: "a clear", method: "delete", path: "/state" },
    ] as const)("answers $request with 404", async ({ method, path }) => {
      const res = await api()
        [method](`/bots/someone-elses${path}`)
        .set(AS_OWNER);

      expectAnswer(res, 404, "Bot not found or access denied");
    });

    it("answers a key read with 404", async () => {
      const res = await api()
        .get("/bots/someone-elses/state/positions")
        .set(AS_OWNER);

      expectAnswer(res, 404, "Bot not found or access denied");
    });
  });

  describe("answer a failed ownership lookup with 500", () => {
    it.each([
      { request: "a list", method: "get", path: "/state" },
      { request: "a key read", method: "get", path: "/state/positions" },
      { request: "a key delete", method: "delete", path: "/state/positions" },
      { request: "a clear", method: "delete", path: "/state" },
    ] as const)("answers $request", async ({ method, path }) => {
      const res = await api()
        [method](`/bots/${UNREADABLE_BOT}${path}`)
        .set(AS_OWNER);

      expectAnswer(res, 500, "Failed to read from the database");
    });
  });

  describe("refuse bad input", () => {
    it("answers an invalid key with 400", async () => {
      const res = await api()
        .get(`/bots/${OWNED_BOT}/state/bad.key`)
        .set(AS_OWNER);

      expectAnswer(res, 400, "Invalid state key");
    });

    it.each([
      { request: "a list", method: "get", path: "/state" },
      { request: "a key read", method: "get", path: "/state/positions" },
      { request: "a key delete", method: "delete", path: "/state/positions" },
      { request: "a clear", method: "delete", path: "/state" },
    ] as const)(
      "answers $request on an invalid bot ID with 400",
      async ({ method, path }) => {
        const res = await api()[method](`/bots/bad.id${path}`).set(AS_OWNER);

        expectAnswer(res, 400, "Invalid bot ID format");
      },
    );
  });

  it("answers a missing key with 404", async () => {
    const res = await api()
      .get(`/bots/${OWNED_BOT}/state/positions`)
      .set(AS_OWNER);

    expectAnswer(res, 404, "State key not found");
  });

  describe("answer a storage failure with 500 and the operation", () => {
    beforeEach(() => {
      minio.statObject.mockRejectedValue(storageDown());
      minio.removeObject.mockRejectedValue(storageDown());
    });

    it.each([
      {
        request: "a list",
        method: "get",
        path: "/state",
        message: "Failed to list state keys",
      },
      {
        request: "a key read",
        method: "get",
        path: "/state/positions",
        message: "Failed to get state key",
      },
      {
        request: "a key delete",
        method: "delete",
        path: "/state/positions",
        message: "Failed to delete state key",
      },
      {
        request: "a clear",
        method: "delete",
        path: "/state",
        message: "Failed to clear state",
      },
    ] as const)("answers $request", async ({ method, path, message }) => {
      const res = await api()
        [method](`/bots/${OWNED_BOT}${path}`)
        .set(AS_OWNER);

      expectAnswer(res, 500, message);
    });
  });
});
