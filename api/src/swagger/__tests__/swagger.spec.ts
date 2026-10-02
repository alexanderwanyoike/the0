import { INestApplication, Type } from "@nestjs/common";
import { GUARDS_METADATA, PATH_METADATA } from "@nestjs/common/constants";
import { ConfigService } from "@nestjs/config";
import { ModulesContainer } from "@nestjs/core";
import { AuthGuard } from "@nestjs/passport";
import { Test } from "@nestjs/testing";
import request from "supertest";
import { version } from "../../../package.json";
import { AppModule } from "@/app.module";
import { AdminBootstrapService } from "@/auth/admin-bootstrap.service";
import { AdminJwtGuard } from "@/auth/admin-jwt.guard";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";
import { JwtAuthGuard } from "@/auth/jwt-auth.guard";
import configuration from "@/config/configuration";
import { NatsService } from "@/nats/nats.service";
import {
  setupSwagger,
  SWAGGER_JSON_PATH,
  SWAGGER_UI_PATH,
} from "../swagger.setup";

type Operation = {
  operationId: string;
  summary?: string;
  tags?: string[];
  security?: Record<string, string[]>[];
  requestBody?: { content: Record<string, unknown> };
};
type OpenApiDocument = {
  openapi: string;
  info: { title: string; version: string };
  paths: Record<string, Record<string, Operation>>;
  components: { securitySchemes: Record<string, Record<string, string>> };
};

const JWT = { bearer: [] as string[] };
const API_KEY = { apiKey: [] as string[] };

const SECURITY_BY_GUARD = new Map<unknown, Record<string, string[]>[]>([
  [AuthCombinedGuard, [JWT, API_KEY]],
  [JwtAuthGuard, [JWT]],
  [AdminJwtGuard, [JWT]],
  [AuthGuard(), [JWT]],
]);

async function createApp(swaggerEnabled: boolean): Promise<INestApplication> {
  const moduleRef = await Test.createTestingModule({ imports: [AppModule] })
    .overrideProvider(NatsService)
    .useValue({})
    .overrideProvider(AdminBootstrapService)
    .useValue({})
    .compile();
  const app = moduleRef.createNestApplication({ logger: false });
  setupSwagger(app, new ConfigService({ SWAGGER_ENABLED: swaggerEnabled }));
  await app.init();
  return app;
}

function operationsById(document: OpenApiDocument): Map<string, Operation> {
  const operations = new Map<string, Operation>();
  for (const pathItem of Object.values(document.paths)) {
    for (const operation of Object.values(pathItem)) {
      operations.set(operation.operationId, operation);
    }
  }
  return operations;
}

function routeHandlers(controller: Type): string[] {
  const prototype = controller.prototype;
  return Object.getOwnPropertyNames(prototype).filter(
    (name) =>
      name !== "constructor" &&
      Reflect.hasMetadata(PATH_METADATA, prototype[name]),
  );
}

describe("Swagger documentation", () => {
  describe("serving the docs", () => {
    let enabledApp: INestApplication;
    let disabledApp: INestApplication;

    beforeAll(async () => {
      enabledApp = await createApp(true);
      disabledApp = await createApp(false);
    });

    afterAll(async () => {
      await enabledApp.close();
      await disabledApp.close();
    });

    it("serves the Swagger UI and the OpenAPI JSON when enabled", async () => {
      const ui = await request(enabledApp.getHttpServer()).get(
        `/${SWAGGER_UI_PATH}`,
      );
      expect(ui.status).toBe(200);
      expect(ui.headers["content-type"]).toMatch(/text\/html/);
      expect(ui.text).toContain("swagger-ui");

      const spec = await request(enabledApp.getHttpServer()).get(
        `/${SWAGGER_JSON_PATH}`,
      );
      expect(spec.status).toBe(200);
      expect(spec.body.openapi).toMatch(/^3\./);
    });

    it("serves neither the UI nor the JSON when disabled", async () => {
      const server = disabledApp.getHttpServer();

      expect((await request(server).get(`/${SWAGGER_UI_PATH}`)).status).toBe(
        404,
      );
      expect((await request(server).get(`/${SWAGGER_JSON_PATH}`)).status).toBe(
        404,
      );
    });
  });

  describe("SWAGGER_ENABLED", () => {
    const originalEnv = { ...process.env };

    afterEach(() => {
      process.env = { ...originalEnv };
    });

    function swaggerEnabled(env: Record<string, string | undefined>) {
      process.env = { ...originalEnv, ...env };
      for (const [key, value] of Object.entries(env)) {
        if (value === undefined) delete process.env[key];
      }
      return configuration().SWAGGER_ENABLED;
    }

    it("is on by default outside production", () => {
      expect(
        swaggerEnabled({ NODE_ENV: "development", SWAGGER_ENABLED: undefined }),
      ).toBe(true);
      expect(
        swaggerEnabled({ NODE_ENV: undefined, SWAGGER_ENABLED: undefined }),
      ).toBe(true);
    });

    it("is off by default in production", () => {
      expect(
        swaggerEnabled({ NODE_ENV: "production", SWAGGER_ENABLED: undefined }),
      ).toBe(false);
    });

    it("turns on in production only when set to true", () => {
      expect(
        swaggerEnabled({ NODE_ENV: "production", SWAGGER_ENABLED: "true" }),
      ).toBe(true);
      expect(
        swaggerEnabled({ NODE_ENV: "production", SWAGGER_ENABLED: "yes" }),
      ).toBe(false);
    });

    it("can be turned off outside production", () => {
      expect(
        swaggerEnabled({ NODE_ENV: "development", SWAGGER_ENABLED: "false" }),
      ).toBe(false);
    });
  });

  describe("the OpenAPI document", () => {
    let app: INestApplication;
    let document: OpenApiDocument;

    beforeAll(async () => {
      app = await createApp(true);
      const res = await request(app.getHttpServer()).get(
        `/${SWAGGER_JSON_PATH}`,
      );
      document = res.body;
    });

    afterAll(async () => {
      await app.close();
    });

    it("names the API and carries the package version", () => {
      expect(document.info.title).toBe("the0 API");
      expect(document.info.version).toBe(version);
    });

    it("declares a bearer JWT scheme and an ApiKey scheme in the Authorization header", () => {
      expect(document.components.securitySchemes.bearer).toMatchObject({
        type: "http",
        scheme: "bearer",
        bearerFormat: "JWT",
      });
      expect(document.components.securitySchemes.apiKey).toMatchObject({
        type: "apiKey",
        in: "header",
        name: "Authorization",
      });
    });

    it.each([
      ["get", "/bot/{id}", "bots", [JWT, API_KEY]],
      ["post", "/custom-bots/{name}", "custom-bots", [JWT, API_KEY]],
      ["get", "/logs/{botId}", "logs", [JWT, API_KEY]],
      ["get", "/bots/{botId}/state", "bot-state", [JWT, API_KEY]],
      ["post", "/query/{botId}", "bot-query", [JWT, API_KEY]],
      ["get", "/api-keys", "api-keys", [JWT]],
      ["get", "/admin/users", "admin", [JWT]],
      ["put", "/users/profile", "users", [JWT]],
      ["get", "/auth/me", "auth", [JWT]],
      ["get", "/auth/validate-api-key", "auth", [API_KEY]],
      ["post", "/auth/login", "auth", undefined],
      ["post", "/mcp", "mcp", undefined],
      ["get", "/health", "health", undefined],
      ["get", "/health/ready", "health", undefined],
    ])(
      "documents %s %s under %s with its security",
      (method, path, tag, security) => {
        const operation = document.paths[path]?.[method];

        expect(operation).toBeDefined();
        expect(operation.tags).toEqual([tag]);
        expect(operation.summary).toEqual(expect.any(String));
        expect(operation.security).toEqual(security);
      },
    );

    it("documents every guarded route with the credentials its guard accepts", () => {
      const operations = operationsById(document);
      const documented: { operationId: string; security: unknown }[] = [];
      const expected: { operationId: string; security: unknown }[] = [];

      for (const module of app.get(ModulesContainer).values()) {
        for (const { metatype } of module.controllers.values()) {
          const controller = metatype as Type;
          const classGuards =
            Reflect.getMetadata(GUARDS_METADATA, controller) ?? [];

          for (const handler of routeHandlers(controller)) {
            const [guard] = [
              ...classGuards,
              ...(Reflect.getMetadata(
                GUARDS_METADATA,
                controller.prototype[handler],
              ) ?? []),
            ];
            if (!guard) continue;

            const operationId = `${controller.name}_${handler}`;
            documented.push({
              operationId,
              security: operations.get(operationId)?.security,
            });
            expected.push({
              operationId,
              security:
                SECURITY_BY_GUARD.get(guard) ?? `unmapped guard ${guard.name}`,
            });
          }
        }
      }

      expect(documented.length).toBeGreaterThan(30);
      expect(documented).toEqual(expected);
    });

    it("documents the bot upload as a multipart form", () => {
      const upload = document.paths["/custom-bots/{name}/upload"].post;

      expect(Object.keys(upload.requestBody.content)).toEqual([
        "multipart/form-data",
      ]);
    });
  });
});
