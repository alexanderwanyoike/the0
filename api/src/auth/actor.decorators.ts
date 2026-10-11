import { SetMetadata } from "@nestjs/common";
import { ActorChannel } from "./actor";

export const ACTOR_VIA_KEY = "auth:actorVia";

/** The channel AuthCombinedGuard records on this route, whichever credential the caller used. */
export const ActorVia = (channel: ActorChannel) =>
  SetMetadata(ACTOR_VIA_KEY, channel);

export const ALLOW_ANONYMOUS_KEY = "auth:allowAnonymous";

/** Requests without valid credentials continue with no Actor; the handler decides what needs one. */
export const AllowAnonymous = () => SetMetadata(ALLOW_ANONYMOUS_KEY, true);
