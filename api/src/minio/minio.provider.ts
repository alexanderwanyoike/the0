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

    const portStr = configService.get<string>("MINIO_PORT") || "9000";
    const port = parseInt(portStr, 10);
    if (isNaN(port)) {
      throw new Error(`MINIO_PORT must be a valid number, got: ${portStr}`);
    }
    const sslValue = configService.get<string>("MINIO_USE_SSL")?.toLowerCase();
    const useSSL = sslValue === "true" || sslValue === "1";

    return new Minio.Client({
      endPoint: endpoint,
      port,
      useSSL,
      accessKey,
      secretKey,
    });
  },
};
