import { INestApplication } from "@nestjs/common";
import request from "supertest";
import { Readable } from "stream";
import { LogsController } from "../logs.controller";
import { LogsService } from "../logs.service";
import { LogStreamService } from "../log-stream.service";
import { LogFanOut } from "../log-fan-out";
import { NatsService } from "@/nats/nats.service";
import {
  AS_OWNER,
  errorBody,
  frontDoor,
  OWNED_BOT,
  UNREADABLE_BOT,
} from "@/test/front-door";

describe("/logs responses", () => {
  let app: INestApplication;
  const minio = {
    statObject: jest.fn(),
    getObject: jest.fn(),
    getPartialObject: jest.fn(),
  };

  beforeAll(async () => {
    app = await frontDoor({
      controllers: [LogsController],
      providers: [
        LogsService,
        LogStreamService,
        LogFanOut,
        { provide: NatsService, useValue: { subscribe: jest.fn() } },
      ],
      minio,
    });
  });

  afterAll(async () => {
    await app.close();
  });

  beforeEach(() => {
    const file = '{"message":"hello"}\n';
    minio.statObject.mockResolvedValue({ size: Buffer.byteLength(file) });
    minio.getObject.mockImplementation(async () =>
      Readable.from([Buffer.from(file)]),
    );
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

  describe("read a page", () => {
    it("answers 200 with the entries", async () => {
      const res = await api()
        .get(`/logs/${OWNED_BOT}?date=20260101`)
        .set(AS_OWNER);

      expect(res.status).toBe(200);
      expect(res.body).toEqual(
        expect.objectContaining({
          success: true,
          data: [expect.objectContaining({ content: '{"message":"hello"}' })],
          message: "Logs retrieved successfully",
        }),
      );
    });

    it("answers a bot the user cannot see with 404", async () => {
      const res = await api()
        .get("/logs/someone-elses?date=20260101")
        .set(AS_OWNER);

      expectAnswer(res, 404, "Bot not found or access denied");
    });

    it("answers an invalid bot ID with 400", async () => {
      const res = await api().get("/logs/bad.id").set(AS_OWNER);

      expectAnswer(res, 400, "Invalid bot ID");
    });

    it("answers a date range it cannot read with 400", async () => {
      const res = await api()
        .get(`/logs/${OWNED_BOT}?dateRange=yesterday`)
        .set(AS_OWNER);

      expectAnswer(
        res,
        400,
        "Invalid date or dateRange format. Use YYYYMMDD, YYYYMMDD-YYYYMMDD, or ISO datetime range with -- separator",
      );
    });

    it("answers a cursor combined with an offset with 400", async () => {
      const res = await api()
        .get(`/logs/${OWNED_BOT}?date=20260101&before=20260101:5&offset=2`)
        .set(AS_OWNER);

      expectAnswer(res, 400, "before cannot be combined with offset");
    });

    it.each(["storage down", "access denied by the bucket policy"])(
      "answers a storage failure (%s) with 500 and the storage message",
      async (message) => {
        minio.getObject.mockRejectedValue(new Error(message));

        const res = await api()
          .get(`/logs/${OWNED_BOT}?date=20260101`)
          .set(AS_OWNER);

        expectAnswer(res, 500, `Failed to fetch logs: ${message}`);
      },
    );
  });

  describe("answer a failed ownership lookup as the failure it is", () => {
    it("answers a read with 500", async () => {
      minio.getObject.mockClear();
      const res = await api()
        .get(`/logs/${UNREADABLE_BOT}?date=20260101`)
        .set(AS_OWNER);

      expectAnswer(res, 500, "Failed to read from the database");
      expect(minio.getObject).not.toHaveBeenCalled();
    });

    it.each(["?history=false", ""])(
      "ends the stream%s with the failure, before going live",
      async (query) => {
        const res = await api()
          .get(`/logs/${UNREADABLE_BOT}/stream${query}`)
          .set(AS_OWNER);

        expect(res.text).toContain(
          'event: error\ndata: {"message":"Failed to read from the database"}',
        );
        expect(res.text).not.toContain(": connected");
      },
    );
  });

  describe("stream", () => {
    it("ends the stream of a bot the user cannot see with an error event", async () => {
      const res = await api()
        .get("/logs/someone-elses/stream?history=false")
        .set(AS_OWNER);

      expect(res.status).toBe(200);
      expect(res.text).toContain(
        'event: error\ndata: {"message":"Bot not found or access denied"}',
      );
    });

    it("ends the history stream of a bot the user cannot see with an error event", async () => {
      const res = await api().get("/logs/someone-elses/stream").set(AS_OWNER);

      expect(res.status).toBe(200);
      expect(res.text).toContain(
        'event: error\ndata: {"message":"Bot not found or access denied"}',
      );
    });
  });
});
