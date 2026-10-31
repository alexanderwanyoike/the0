import { ConfigService } from "@nestjs/config";

/** A ConfigService with nothing set, so every service uses its defaults. */
export const unsetConfig = (): ConfigService =>
  ({ get: (): undefined => undefined }) as unknown as ConfigService;
