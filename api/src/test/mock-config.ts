import { ConfigService } from "@nestjs/config";
import configuration from "@/config/configuration";

/**
 * A ConfigService as the app loads it with no settings of its own, so every
 * service sees the defaults from `configuration.ts`, as in production.
 */
export const unsetConfig = (): ConfigService => {
  const values: Record<string, unknown> = configuration();
  return { get: (key: string) => values[key] } as unknown as ConfigService;
};
