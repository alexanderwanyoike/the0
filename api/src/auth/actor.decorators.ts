import { SetMetadata } from "@nestjs/common";
import { ActorChannel } from "./actor";

export const ACTOR_VIA_KEY = "auth:actorVia";

/** The channel AuthCombinedGuard records on this route, whichever credential the caller used. */
export const ActorVia = (channel: ActorChannel) =>
  SetMetadata(ACTOR_VIA_KEY, channel);
