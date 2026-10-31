import {
  BadRequestException,
  HttpException,
  InternalServerErrorException,
  NotFoundException,
} from "@nestjs/common";
import { AppError, ErrorCode } from "./app-error";

const EXCEPTIONS: Record<ErrorCode, new (message: string) => HttpException> = {
  validation: BadRequestException,
  // Not 403: the CLI reads any 403 as a revoked API key and asks the user
  // to log in again.
  forbidden: BadRequestException,
  not_found: NotFoundException,
  internal: InternalServerErrorException,
};

/** The REST answer to an AppError, with the body Nest gives its exceptions. */
export function toHttpException(error: AppError): HttpException {
  return new EXCEPTIONS[error.code](error.message);
}
