import { BotConfig } from "@/database/schema/bots";

export class Bot {
  name: string;
  id: string;
  config: BotConfig;
  topic: string;
  createdAt: Date;
  updatedAt: Date;
  userId: string;
  customBotId: string;
}
