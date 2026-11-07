import { ExecutionContext, INestApplication } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { Test } from "@nestjs/testing";
import { createHash } from "crypto";
import { mkdtemp, readdir, rm } from "fs/promises";
import { request as httpRequest } from "http";
import { AddressInfo } from "net";
import os from "os";
import { join } from "path";
import { Readable } from "stream";
import request from "supertest";
import { PinoLogger } from "nestjs-pino";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";
import { MINIO_CLIENT } from "@/minio";
import { createMockLogger } from "@/test/mock-logger";
import { CustomBotService } from "../custom-bot.service";
import { StorageService } from "../storage.service";

describe("custom bot uploads", () => {
  let app: INestApplication;
  let uploadDir: string;
  let restoreTempDirectory: jest.SpyInstance;
  const payload = Buffer.alloc(1024 * 1024, "zip contents");
  const minio = {
    bucketExists: jest.fn().mockResolvedValue(true),
    putObject: jest.fn(),
  };
  let received: { path: string; size: number; digest: string }[];

  beforeAll(async () => {
    uploadDir = await mkdtemp(join(os.tmpdir(), "the0-upload-test-"));
    restoreTempDirectory = jest.spyOn(os, "tmpdir").mockReturnValue(uploadDir);
    const { CustomBotController } = await import("../custom-bot.controller");
    const moduleRef = await Test.createTestingModule({
      controllers: [CustomBotController],
      providers: [
        StorageService,
        { provide: CustomBotService, useValue: {} },
        { provide: MINIO_CLIENT, useValue: minio },
        { provide: ConfigService, useValue: { get: () => "custom-bots" } },
        { provide: PinoLogger, useValue: createMockLogger() },
      ],
    })
      .overrideGuard(AuthCombinedGuard)
      .useValue({
        canActivate(context: ExecutionContext) {
          context.switchToHttp().getRequest().user = { uid: "user123" };
          return true;
        },
      })
      .compile();
    app = moduleRef.createNestApplication();
    app.useLogger(false);
    await app.listen(0, "127.0.0.1");
  });

  afterAll(async () => {
    await app?.close();
    restoreTempDirectory?.mockRestore();
    if (uploadDir) await rm(uploadDir, { recursive: true, force: true });
  });

  beforeEach(() => {
    received = [];
    minio.putObject.mockReset();
    minio.putObject.mockImplementation(
      async (
        _bucket: string,
        path: string,
        content: Buffer | Readable,
        size: number,
      ) => {
        const hash = createHash("sha256");
        if (Buffer.isBuffer(content)) {
          hash.update(content);
        } else {
          for await (const chunk of content) hash.update(chunk);
        }
        received.push({ path, size, digest: hash.digest("hex") });
        return { etag: "test-etag" };
      },
    );
  });

  afterEach(async () => {
    expect(await readdir(uploadDir)).toEqual([]);
  });

  const upload = () =>
    request(app.getHttpServer()).post("/custom-bots/test-bot/upload");

  it.each(["before", "after"])(
    "streams the ZIP with its size when version is sent %s the file",
    async (order) => {
      const req = upload();
      if (order === "before") req.field("version", "1.0.0");
      req.attach("file", payload, "bot.zip");
      if (order === "after") req.field("version", "1.0.0");
      const res = await req;

      expect(res.status).toBe(200);
      expect(res.body).toEqual({
        success: true,
        filePath: "user123/test-bot/1.0.0",
        message: "File uploaded successfully",
      });
      expect(minio.putObject.mock.calls[0][2] instanceof Readable).toBe(true);
      expect(received).toEqual([
        {
          path: "user123/test-bot/1.0.0",
          size: payload.length,
          digest: createHash("sha256").update(payload).digest("hex"),
        },
      ]);
      expect(minio.putObject.mock.calls[0][4]).toMatchObject({
        "Content-Type": "application/zip",
        "X-Upload-Source": "custom-bot-api",
      });
    },
  );

  it("rejects a missing file", async () => {
    const res = await upload().field("version", "1.0.0");
    expect(res.status).toBe(400);
    expect(res.body.message).toBe("File is required");
    expect(minio.putObject).not.toHaveBeenCalled();
  });

  it("uploads an empty file without waiting for data", async () => {
    const res = await upload()
      .field("version", "1.0.0")
      .attach("file", Buffer.alloc(0), "empty.zip");
    expect(res.status).toBe(200);
    expect(received[0].size).toBe(0);
    expect(received[0].digest).toBe(createHash("sha256").digest("hex"));
  });

  it("removes the temporary file when the version is missing", async () => {
    const res = await upload().attach("file", payload, "bot.zip");
    expect(res.status).toBe(400);
    expect(res.body.message).toBe("Version is required");
    expect(minio.putObject).not.toHaveBeenCalled();
  });

  it("removes the temporary file when the file type is rejected", async () => {
    const res = await upload()
      .field("version", "1.0.0")
      .attach("file", payload, "bot.txt");
    expect(res.status).toBe(400);
    expect(res.body.message).toBe("Only ZIP files are allowed");
    expect(minio.putObject).not.toHaveBeenCalled();
  });

  it.each(["before", "during"])(
    "closes the file and removes it when storage fails %s reading",
    async (when) => {
      minio.putObject.mockImplementation(
        async (_bucket: string, _path: string, content: Readable) => {
          if (when === "during") await content[Symbol.asyncIterator]().next();
          throw new Error("storage unavailable");
        },
      );
      const res = await upload()
        .field("version", "1.0.0")
        .attach("file", payload, "bot.zip");
      expect(res.status).toBe(500);
      expect(res.body.message).toBe(
        "Upload failed: Failed to write to storage",
      );
      const content = minio.putObject.mock.calls[0][2];
      expect(content instanceof Readable).toBe(true);
      expect(content).toMatchObject({
        destroyed: true,
        closed: true,
      });
    },
  );

  it("cleans up a read error even if the storage client does not reject", async () => {
    minio.putObject.mockImplementation(
      (_bucket: string, _path: string, content: Readable) => {
        queueMicrotask(() => content.destroy(new Error("disk read failed")));
        return new Promise<never>(() => {});
      },
    );
    const res = await upload()
      .field("version", "1.0.0")
      .attach("file", payload, "bot.zip");
    expect(res.status).toBe(500);
    expect(res.body.message).toBe("Upload failed: Failed to write to storage");
    expect(minio.putObject.mock.calls[0][2]).toMatchObject({
      destroyed: true,
      closed: true,
    });
  });

  it("cleans up when multipart parsing rejects a second file", async () => {
    const res = await upload()
      .field("version", "1.0.0")
      .attach("file", payload, "bot.zip")
      .attach("file", payload, "other.zip");
    // Nest currently maps Multer's unexpected-file error to 500.
    expect(res.status).toBe(500);
    expect(minio.putObject).not.toHaveBeenCalled();
  });

  const incompleteMultipart = () =>
    Buffer.concat([
      Buffer.from(
        '--upload-boundary\r\nContent-Disposition: form-data; name="file"; filename="bot.zip"\r\nContent-Type: application/zip\r\n\r\n',
      ),
      payload,
    ]);

  it("cleans up a multipart body with no closing boundary", async () => {
    const res = await upload()
      .set("Content-Type", "multipart/form-data; boundary=upload-boundary")
      .send(incompleteMultipart());
    expect(res.status).toBeGreaterThanOrEqual(400);
    expect(minio.putObject).not.toHaveBeenCalled();
  });

  it("removes a partial file when the client disconnects", async () => {
    const waitForFileCount = async (count: number) => {
      for (let attempt = 0; attempt < 200; attempt++) {
        if ((await readdir(uploadDir)).length === count) return;
        await new Promise((resolve) => setTimeout(resolve, 10));
      }
      expect(await readdir(uploadDir)).toHaveLength(count);
    };
    const { port } = app.getHttpServer().address() as AddressInfo;
    const req = httpRequest({
      hostname: "127.0.0.1",
      port,
      path: "/custom-bots/test-bot/upload",
      method: "POST",
      headers: {
        "Content-Type": "multipart/form-data; boundary=upload-boundary",
      },
    });
    req.on("error", (): undefined => undefined);
    try {
      req.write(incompleteMultipart());
      await waitForFileCount(1);
    } finally {
      req.destroy();
    }
    await waitForFileCount(0);
    expect(minio.putObject).not.toHaveBeenCalled();
  });

  it("handles concurrent uploads with identical filenames independently", async () => {
    const responses = await Promise.all(
      ["1.0.0", "2.0.0"].map((version) =>
        upload().field("version", version).attach("file", payload, "bot.zip"),
      ),
    );
    expect(responses.map((res) => res.status)).toEqual([200, 200]);
    expect(received.map((file) => file.path).sort()).toEqual([
      "user123/test-bot/1.0.0",
      "user123/test-bot/2.0.0",
    ]);
    expect(received.every((file) => file.size === payload.length)).toBe(true);
  });
});
