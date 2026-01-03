import { Global, Module } from "@nestjs/common";
import { ConfigModule } from "@nestjs/config";
import { MinioClientProvider, MINIO_CLIENT } from "./minio.provider";

@Global()
@Module({
  imports: [ConfigModule],
  providers: [MinioClientProvider],
  exports: [MINIO_CLIENT],
})
export class MinioModule {}
