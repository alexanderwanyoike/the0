import { HttpException } from "@nestjs/common";
import { appError, ErrorCode } from "../app-error";
import { toHttpException } from "../http-error";

describe("toHttpException", () => {
  it.each<[ErrorCode, number, string]>([
    ["validation", 400, "Bad Request"],
    ["forbidden", 400, "Bad Request"],
    ["not_found", 404, "Not Found"],
    ["internal", 500, "Internal Server Error"],
  ])("answers %s with %i and Nest's usual body", (code, status, statusText) => {
    const exception = toHttpException(appError(code, "The reason"));

    expect(exception).toBeInstanceOf(HttpException);
    expect(exception.getStatus()).toBe(status);
    expect(exception.getResponse()).toEqual({
      statusCode: status,
      message: "The reason",
      error: statusText,
    });
  });
});
