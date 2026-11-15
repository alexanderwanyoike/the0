import {
  CallHandler,
  ExecutionContext,
  Inject,
  Injectable,
} from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { FileInterceptor } from "@nestjs/platform-express";
import { MulterError, diskStorage } from "multer";
import { Observable } from "rxjs";
import { appError } from "@/common/app-error";
import { toHttpException } from "@/common/http-error";

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

  async intercept(
    context: ExecutionContext,
    next: CallHandler,
  ): Promise<Observable<unknown>> {
    try {
      return await super.intercept(context, next);
    } catch (error) {
      // Nest maps Multer errors by message and misses ones it doesn't know,
      // such as "Unexpected file field", which would otherwise surface as 500.
      if (error instanceof MulterError) {
        throw toHttpException(appError("validation", error.message));
      }
      throw error;
    }
  }
}
