import {
  createParamDecorator,
  ExecutionContext,
  SetMetadata,
} from "@nestjs/common";
import { Actor, ActorChannel } from "./actor";
import { AuthenticatedRequest } from "./auth.types";

export const CurrentActor = createParamDecorator(
  (_data: unknown, ctx: ExecutionContext): Actor | undefined =>
    ctx.switchToHttp().getRequest<AuthenticatedRequest>().actor,
);

export const ACTOR_VIA_KEY = "auth:actorVia";

/** The channel AuthCombinedGuard records on this route, whichever credential the caller used. */
export const ActorVia = (channel: ActorChannel) =>
  SetMetadata(ACTOR_VIA_KEY, channel);

export const ALLOW_ANONYMOUS_KEY = "auth:allowAnonymous";

/** Requests without valid credentials continue with no Actor; the handler decides what needs one. */
export const AllowAnonymous = () => SetMetadata(ALLOW_ANONYMOUS_KEY, true);
