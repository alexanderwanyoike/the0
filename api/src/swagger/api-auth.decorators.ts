import { applyDecorators } from "@nestjs/common";
import {
  ApiBearerAuth,
  ApiForbiddenResponse,
  ApiSecurity,
  ApiUnauthorizedResponse,
} from "@nestjs/swagger";

export const BEARER_AUTH = "bearer";
export const API_KEY_AUTH = "apiKey";

const Unauthorized = () =>
  ApiUnauthorizedResponse({ description: "Missing or invalid credentials" });

/** Documents routes behind JwtAuthGuard or the passport AuthGuard(). */
export const ApiJwtAuth = () =>
  applyDecorators(ApiBearerAuth(BEARER_AUTH), Unauthorized());

/** Documents routes behind AdminJwtGuard. */
export const ApiAdminJwtAuth = () =>
  applyDecorators(
    ApiJwtAuth(),
    ApiForbiddenResponse({ description: "Caller is not an admin" }),
  );

/** Documents routes behind AuthCombinedGuard: either credential is enough. */
export const ApiJwtOrApiKeyAuth = () =>
  applyDecorators(
    ApiBearerAuth(BEARER_AUTH),
    ApiSecurity(API_KEY_AUTH),
    Unauthorized(),
  );

export const ApiKeySecurity = () =>
  applyDecorators(ApiSecurity(API_KEY_AUTH), Unauthorized());
