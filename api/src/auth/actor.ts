export type ActorChannel = "jwt" | "api-key" | "mcp";

/** The authenticated caller a change is made for. */
export interface Actor {
  userId: string;
  via: ActorChannel;
  /** Set whenever an API key authenticated the caller, whatever the channel. */
  apiKeyId?: string;
}
