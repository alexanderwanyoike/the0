import { CommandBus } from "@/command-bus/command-bus";
import { BotConfigResolver } from "@/bot/commands/bot-config.resolver";
import { BotEventFactory } from "@/bot/commands/bot-event-factory";
import { DeployBotHandler } from "@/bot/commands/deploy-bot.handler";
import { UpdateBotHandler } from "@/bot/commands/update-bot.handler";
import { DeleteBotHandler } from "@/bot/commands/delete-bot.handler";

/**
 * The CommandBus with the real Bot command handlers. The handlers register
 * on module init, so call init() on the testing module before dispatching.
 */
export const BOT_COMMAND_PROVIDERS = [
  CommandBus,
  BotConfigResolver,
  BotEventFactory,
  DeployBotHandler,
  UpdateBotHandler,
  DeleteBotHandler,
];
