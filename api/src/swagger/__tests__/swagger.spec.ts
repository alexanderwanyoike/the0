import { INestApplication } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { Test } from "@nestjs/testing";
import request from "supertest";
import { version } from "../../../package.json";
import { AppModule } from "@/app.module";
import { AdminBootstrapService } from "@/auth/admin-bootstrap.service";
import configuration from "@/config/configuration";
import { NatsService } from "@/nats/nats.service";
import {
  setupSwagger,
  SWAGGER_JSON_PATH,
  SWAGGER_UI_PATH,
} from "../swagger.setup";

type OpenApiDocument = {
  openapi: string;
  info: { title: string; version: string };
  components: { securitySchemes: Record<string, Record<string, string>> };
};

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
  });
});
