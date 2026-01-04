import { Provider } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import * as Minio from "minio";

export const MINIO_CLIENT = "MINIO_CLIENT";

export const MinioClientProvider: Provider = {
  provide: MINIO_CLIENT,
  inject: [ConfigService],
  useFactory: (configService: ConfigService): Minio.Client => {
    const endpoint = configService.get<string>("MINIO_ENDPOINT");
    const accessKey = configService.get<string>("MINIO_ACCESS_KEY");
    const secretKey = configService.get<string>("MINIO_SECRET_KEY");

    // Fail loudly if credentials are missing
    if (!endpoint) {
      throw new Error("MINIO_ENDPOINT is required but not configured");
    }
    if (!accessKey) {
      throw new Error("MINIO_ACCESS_KEY is required but not configured");
    }
    if (!secretKey) {
      throw new Error("MINIO_SECRET_KEY is required but not configured");
    }

    const port = parseInt(configService.get<string>("MINIO_PORT") || "9000");
    const useSSL = configService.get<string>("MINIO_USE_SSL") === "true";

    return new Minio.Client({
      endPoint: endpoint,
      port,
      useSSL,
      accessKey,
      secretKey,
    });
  },
};
