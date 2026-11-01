import { INestApplication, ValidationPipe } from "@nestjs/common";
import { Test } from "@nestjs/testing";
import { PinoLogger } from "nestjs-pino";
import request from "supertest";
import { CustomBotController } from "../custom-bot.controller";
import { CustomBotRepository } from "../custom-bot.repository";
import { CustomBotService } from "../custom-bot.service";
import { StorageService } from "../storage.service";
import { ApiKeyService } from "@/api-key/api-key.service";
import { AuthService } from "@/auth/auth.service";
import { appError } from "@/common/app-error";
import { Failure, Ok } from "@/common/result";
import { botsTable } from "@/database/schema/bots";
import { usersTable } from "@/database/schema/users";
import { mockBotConfig } from "@/test/mock-bot";
import { createMockLogger } from "@/test/mock-logger";
import { TEST_OWNER_ID, useTestDatabase } from "@/test/test-database";

jest.mock(
  "@/database/connection",
  () => jest.requireActual("@/test/test-database").connection,
);

const UPLOADER = TEST_OWNER_ID;
const OTHER_USER = "user-2";

const customBotConfig = (overrides: Record<string, unknown> = {}) => ({
  name: "sma-bot",
  description: "SMA crossover",
  version: "1.0.0",
  type: "scheduled",
  runtime: "python3.11",
  author: "owner",
  entrypoints: { bot: "main.py" },
  schema: { bot: { type: "object" } },
  readme: "A simple moving average crossover bot, for the response tests.",
  ...overrides,
});

describe("/custom-bots responses", () => {
  const db = useTestDatabase();
  let app: INestApplication;
  const storage = {
    fileExists: jest.fn(),
    validateZipStructure: jest.fn(),
    extractAndStoreFrontend: jest.fn(),
    listObjects: jest.fn(),
    deleteFile: jest.fn(),
    uploadBotFile: jest.fn(),
    getFrontendPath: jest.fn(),
    getBotFrontendStream: jest.fn(),
  };

  beforeAll(async () => {
    const moduleRef = await Test.createTestingModule({
      controllers: [CustomBotController],
      providers: [
        CustomBotService,
        CustomBotRepository,
        { provide: StorageService, useValue: storage },
        {
          provide: ApiKeyService,
          useValue: {
            validateApiKey: async (key: string) =>
              Ok({ id: key, userId: key, name: "cli" }),
          },
        },
        { provide: AuthService, useValue: { validateToken: jest.fn() } },
        { provide: PinoLogger, useValue: createMockLogger() },
      ],
    }).compile();

    app = moduleRef.createNestApplication();
    // As main.ts configures it.
    app.useGlobalPipes(
      new ValidationPipe({
        whitelist: true,
        forbidNonWhitelisted: true,
        transform: true,
      }),
    );
    await app.init();
  });

  afterAll(async () => {
    await app.close();
  });

  beforeEach(async () => {
    jest.restoreAllMocks();
    storage.fileExists.mockResolvedValue(Ok(true));
    storage.validateZipStructure.mockResolvedValue(Ok(true));
    storage.extractAndStoreFrontend.mockResolvedValue(Ok(null));
    storage.listObjects.mockResolvedValue(Ok([]));
    storage.deleteFile.mockResolvedValue(Ok(null));
    await db().insert(usersTable).values({
      id: OTHER_USER,
      username: "other",
      email: "other@example.com",
      passwordHash: "hash",
    });
  });

  const api = () => request(app.getHttpServer());
  const as = (userId: string) => ({ "x-api-key": userId });

  const publish = (
    method: "post" | "put",
    userId: string,
    config: Record<string, unknown>,
  ) =>
    api()
      [method](`/custom-bots/${config.name}`)
      .set(as(userId))
      .send({
        config: JSON.stringify(config),
        filePath: `${userId}/${config.name}/${config.version}.zip`,
      });

  const uploaded = async () => {
    const res = await publish("post", UPLOADER, customBotConfig());
    expect(res.status).toBe(201);
    return res.body.data.id as string;
  };

  const uploadedWithFrontend = async () => {
    storage.extractAndStoreFrontend.mockResolvedValue(
      Ok(`${UPLOADER}/sma-bot/1.0.0/frontend.js`),
    );
    return uploaded();
  };

  const expectRefusal = (
    res: request.Response,
    status: number,
    message: string,
  ) => {
    const error = {
      400: "Bad Request",
      404: "Not Found",
      500: "Internal Server Error",
    }[status];
    expect({ status: res.status, body: res.body }).toEqual({
      status,
      body: { statusCode: status, message, error },
    });
  };

  describe("publish the first version", () => {
    it("answers 201 with the stored version", async () => {
      const res = await publish("post", UPLOADER, customBotConfig());

      expect(res.status).toBe(201);
      expect(res.body).toEqual({
        success: true,
        data: expect.objectContaining({
          name: "sma-bot",
          version: "1.0.0",
          userId: UPLOADER,
        }),
        message: "Custom bot created successfully",
      });
    });

    it("refuses a config that is not JSON", async () => {
      const res = await api()
        .post("/custom-bots/sma-bot")
        .set(as(UPLOADER))
        .send({ config: "{", filePath: `${UPLOADER}/sma-bot/1.0.0.zip` });

      expectRefusal(res, 400, "Config must be valid JSON");
    });

    it("refuses a config named differently from the path", async () => {
      const res = await api()
        .post("/custom-bots/other-bot")
        .set(as(UPLOADER))
        .send({
          config: JSON.stringify(customBotConfig()),
          filePath: `${UPLOADER}/sma-bot/1.0.0.zip`,
        });

      expectRefusal(res, 400, "Bot name in config must match URL parameter");
    });

    it("refuses a file uploaded by another user", async () => {
      const res = await api()
        .post("/custom-bots/sma-bot")
        .set(as(UPLOADER))
        .send({
          config: JSON.stringify(customBotConfig()),
          filePath: `${OTHER_USER}/sma-bot/1.0.0.zip`,
        });

      expectRefusal(
        res,
        400,
        "File path must belong to the authenticated user",
      );
    });

    it("refuses a config the Custom bot schema rejects", async () => {
      const res = await publish(
        "post",
        UPLOADER,
        customBotConfig({ readme: "short" }),
      );

      expect(res.status).toBe(400);
      expect(res.body.message).toMatch(/^Validation failed: /);
    });

    it.each(["-sma-bot", "sma-bot-", "sma--bot"])(
      "refuses the name %s, which no bot type could name",
      async (name) => {
        const res = await publish("post", UPLOADER, customBotConfig({ name }));

        expectRefusal(
          res,
          400,
          "Validation failed: /name must be lowercase letters and numbers, with single hyphens between them",
        );
      },
    );

    it.each(["my-bot", "bot2"])("accepts the name %s", async (name) => {
      const res = await publish("post", UPLOADER, customBotConfig({ name }));

      expect(res.status).toBe(201);
    });

    it("refuses a name already in the Catalogue, whoever uploaded it", async () => {
      await uploaded();

      const res = await publish("post", OTHER_USER, customBotConfig());

      expectRefusal(res, 400, "Custom bot with this name already exists");
    });
  });

  describe("publish a new version", () => {
    it("refuses a user who is not the Uploader", async () => {
      await uploaded();

      const res = await publish(
        "put",
        OTHER_USER,
        customBotConfig({ version: "1.1.0" }),
      );

      expectRefusal(res, 400, "Insufficient permissions");
    });

    it("refuses a version that is not newer", async () => {
      await uploaded();

      const res = await publish("put", UPLOADER, customBotConfig());

      expectRefusal(
        res,
        400,
        "Version 1.0.0 must be greater than current version 1.0.0",
      );
    });

    it("refuses a Custom bot missing from the Catalogue with 404", async () => {
      const res = await publish("put", UPLOADER, customBotConfig());

      expectRefusal(
        res,
        404,
        "Custom bot does not exist. Create it first using POST.",
      );
    });
  });

  describe("read the Catalogue", () => {
    it("shows every user the versions of any Custom bot", async () => {
      await uploaded();

      const versions = await api()
        .get("/custom-bots/sma-bot")
        .set(as(OTHER_USER));
      const version = await api()
        .get("/custom-bots/sma-bot/1.0.0")
        .set(as(OTHER_USER));

      expect(versions.status).toBe(200);
      expect(versions.body.data.latestVersion).toBe("1.0.0");
      expect(version.status).toBe(200);
      expect(version.body.data.version).toBe("1.0.0");
    });

    it("answers a Custom bot missing from the Catalogue with 404", async () => {
      const res = await api().get("/custom-bots/nope").set(as(UPLOADER));

      expectRefusal(res, 404, "Bot not found");
    });

    it("answers a missing version with 404", async () => {
      await uploaded();

      const res = await api()
        .get("/custom-bots/sma-bot/9.9.9")
        .set(as(UPLOADER));

      expectRefusal(res, 404, "Not found");
    });

    it("answers the versions of a Custom bot the user did not upload with 404", async () => {
      await uploaded();

      const res = await api()
        .get("/custom-bots/sma-bot/versions")
        .set(as(OTHER_USER));

      expectRefusal(res, 404, "Bot not found");
    });

    it("answers a missing frontend bundle with 404", async () => {
      const res = await api()
        .get("/custom-bots/by-id/missing/frontend")
        .set(as(UPLOADER));

      expectRefusal(res, 404, "Custom bot not found: missing");
    });

    it("answers a version without a frontend with 404", async () => {
      const id = await uploaded();

      const res = await api()
        .get(`/custom-bots/by-id/${id}/frontend`)
        .set(as(UPLOADER));

      expectRefusal(res, 404, "This bot does not have a custom frontend");
    });

    it("answers a frontend bundle missing from storage with 404", async () => {
      const id = await uploadedWithFrontend();
      storage.getBotFrontendStream.mockResolvedValue(
        Failure(appError("not_found", "Frontend bundle not found")),
      );

      const res = await api()
        .get(`/custom-bots/by-id/${id}/frontend`)
        .set(as(UPLOADER));

      expectRefusal(res, 404, "Frontend bundle not found");
    });
  });

  describe("delete versions", () => {
    it("deletes a version no bot uses", async () => {
      await uploaded();

      const res = await api()
        .delete("/custom-bots/sma-bot/1.0.0")
        .set(as(UPLOADER));

      expect(res.status).toBe(200);
      expect(res.body).toEqual({
        success: true,
        message: "Version 1.0.0 of 'sma-bot' deleted successfully",
      });
    });

    it("refuses a user who is not the Uploader", async () => {
      await uploaded();

      const res = await api()
        .delete("/custom-bots/sma-bot/1.0.0")
        .set(as(OTHER_USER));

      expectRefusal(res, 400, "Insufficient permissions");
    });

    it("refuses a version a bot runs", async () => {
      const customBotId = await uploaded();
      await db()
        .insert(botsTable)
        .values({
          id: "bot-1",
          userId: OTHER_USER,
          name: "SMA",
          config: mockBotConfig({ type: "scheduled/sma-bot" }),
          topic: "the0-scheduled-custom-bot",
          customBotId,
        });

      const res = await api()
        .delete("/custom-bots/sma-bot/1.0.0")
        .set(as(UPLOADER));

      expectRefusal(
        res,
        400,
        "Cannot delete version 1.0.0: 1 active instance(s) reference it",
      );
    });

    it.each([
      {
        target: "a version of a missing Custom bot",
        path: "/custom-bots/nope/1.0.0",
        message: "Bot not found",
      },
      {
        target: "a missing version",
        path: "/custom-bots/sma-bot/9.9.9",
        message: "Not found",
      },
      {
        target: "every version of a missing Custom bot",
        path: "/custom-bots/nope",
        message: "Bot not found",
      },
    ])("refuses $target with 404", async ({ path, message }) => {
      await uploaded();

      const res = await api().delete(path).set(as(UPLOADER));

      expectRefusal(res, 404, message);
    });
  });

  describe("answer a storage or database failure with 500", () => {
    const failingDatabase = () => {
      jest.spyOn(db(), "select").mockImplementation(() => {
        throw new Error("connection lost to 10.0.0.5");
      });
    };

    it.each([
      { request: "the user's Custom bots", path: "/custom-bots" },
      { request: "a Custom bot's versions", path: "/custom-bots/sma-bot" },
      { request: "one version", path: "/custom-bots/sma-bot/1.0.0" },
      {
        request: "the versions with instance counts",
        path: "/custom-bots/sma-bot/versions",
      },
      {
        request: "a frontend bundle",
        path: "/custom-bots/by-id/cb-1/frontend",
      },
    ])("answers a read of $request", async ({ path }) => {
      failingDatabase();

      const res = await api().get(path).set(as(UPLOADER));

      expectRefusal(res, 500, "Failed to read from the database");
    });

    const driverError = new Error("connect ECONNREFUSED 10.0.0.7:9000");

    // Routes the named calls through the real StorageService over a MinIO
    // client that cannot be reached, so the response shows what the service
    // makes of the driver's error.
    const unreachableStorage = (...methods: (keyof typeof storage)[]) => {
      const minio = {
        bucketExists: jest.fn().mockResolvedValue(true),
        statObject: jest.fn().mockRejectedValue(driverError),
        getObject: jest.fn().mockRejectedValue(driverError),
        putObject: jest.fn().mockRejectedValue(driverError),
      };
      const logger = createMockLogger();
      const real = new StorageService(
        minio as any,
        { get: (): undefined => undefined } as any,
        logger as any,
      );
      for (const method of methods) {
        storage[method].mockImplementation((...args: unknown[]) =>
          (real[method] as (...a: unknown[]) => unknown)(...args),
        );
      }
      return logger;
    };

    const expectDriverErrorLogged = (
      logger: ReturnType<typeof createMockLogger>,
    ) =>
      expect(logger.error).toHaveBeenCalledWith(
        expect.objectContaining({ err: driverError }),
        expect.any(String),
      );

    it("answers a frontend bundle the storage fails to read", async () => {
      const id = await uploadedWithFrontend();
      const logger = unreachableStorage("getBotFrontendStream");

      const res = await api()
        .get(`/custom-bots/by-id/${id}/frontend`)
        .set(as(UPLOADER));

      expectRefusal(res, 500, "Failed to read from storage");
      expectDriverErrorLogged(logger);
    });

    it("answers an upload the storage refuses", async () => {
      const logger = unreachableStorage("uploadBotFile");

      const res = await api()
        .post("/custom-bots/sma-bot/upload")
        .set(as(UPLOADER))
        .field("version", "1.0.0")
        .attach("file", Buffer.from("PK"), "sma-bot.zip");

      expectRefusal(res, 500, "Upload failed: Failed to write to storage");
      expectDriverErrorLogged(logger);
    });

    it("answers a deploy whose file check fails", async () => {
      const logger = unreachableStorage("fileExists");

      const res = await publish("post", UPLOADER, customBotConfig());

      expectRefusal(
        res,
        500,
        "File validation failed: Failed to read from storage",
      );
      expectDriverErrorLogged(logger);
    });

    it("answers a deploy whose ZIP read fails", async () => {
      const logger = unreachableStorage("validateZipStructure");

      const res = await publish("post", UPLOADER, customBotConfig());

      expectRefusal(
        res,
        500,
        "ZIP validation failed: Failed to read from storage",
      );
      expectDriverErrorLogged(logger);
    });
  });
});
