import { Inject, Injectable } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { FileInterceptor } from "@nestjs/platform-express";
import { diskStorage } from "multer";

/** Bound temporary disk use before a ZIP can reach the storage service. */
@Injectable()
export class CustomBotUploadInterceptor extends FileInterceptor("file", {
  storage: diskStorage({}),
}) {
  constructor(@Inject(ConfigService) config: ConfigService) {
    super({
      limits: { fileSize: config.get<number>("CUSTOM_BOT_UPLOAD_MAX_BYTES") },
    });
  }
}
