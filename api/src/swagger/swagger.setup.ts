import { INestApplication } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { DocumentBuilder, SwaggerModule } from "@nestjs/swagger";
import { version } from "../../package.json";
import { API_KEY_AUTH, BEARER_AUTH } from "./api-auth.decorators";

export const SWAGGER_UI_PATH = "docs";
export const SWAGGER_JSON_PATH = "docs-json";

function buildDocumentConfig() {
  return new DocumentBuilder()
    .setTitle("the0 API")
    .setDescription(
      "REST API for the0, the runtime platform for algorithmic trading bots. " +
        "Authenticate with a JWT from `POST /auth/login`, or with an API key " +
        "from `POST /api-keys` sent as `Authorization: ApiKey <key>` (what " +
        "the the0 CLI sends).",
    )
    .setVersion(version)
    .setLicense("Apache-2.0", "https://www.apache.org/licenses/LICENSE-2.0")
    .addBearerAuth(
      {
        type: "http",
        scheme: "bearer",
        bearerFormat: "JWT",
        description: "JWT returned by `POST /auth/login`.",
      },
      BEARER_AUTH,
    )
    .addApiKey(
      {
        type: "apiKey",
        in: "header",
        name: "Authorization",
        description:
          "API key sent as `Authorization: ApiKey <key>`. The header value " +
          "is sent verbatim, so enter `ApiKey <key>`, not just the key.",
      },
      API_KEY_AUTH,
    )
    .build();
}

export function setupSwagger(
  app: INestApplication,
  config: ConfigService,
): boolean {
  if (config.get<boolean>("SWAGGER_ENABLED") !== true) {
    return false;
  }

  SwaggerModule.setup(
    SWAGGER_UI_PATH,
    app,
    () =>
      SwaggerModule.createDocument(app, buildDocumentConfig(), {
        // UserController tags admin and users routes per method; an extra
        // controller-name tag would list each of them twice.
        autoTagControllers: false,
      }),
    {
      jsonDocumentUrl: SWAGGER_JSON_PATH,
      raw: ["json"],
      customSiteTitle: "the0 API",
      swaggerOptions: { persistAuthorization: true },
    },
  );
  return true;
}
