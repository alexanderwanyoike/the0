import { appError, AppError } from "@/common/app-error";

export const ownedBotReadError = (error: AppError): AppError =>
  error.code === "not_found" ? appError("not_found", "Bot not found") : error;
