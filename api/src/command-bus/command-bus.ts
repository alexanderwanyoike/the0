import { Injectable } from "@nestjs/common";
import { Command, CommandHandler, ResultOf } from "./command";

// eslint-disable-next-line @typescript-eslint/no-explicit-any -- any constructor signature
type CommandClass<C> = new (...args: any[]) => C;

/**
 * Routes each command to the one handler registered for its class. Every
 * change goes through execute, so this is where audit and a kill switch
 * will hook in.
 */
@Injectable()
export class CommandBus {
  private readonly handlers = new Map<
    CommandClass<Command<unknown>>,
    CommandHandler<Command<unknown>>
  >();

  register<C extends Command<unknown>>(
    type: CommandClass<C>,
    handler: CommandHandler<C>,
  ): void {
    if (this.handlers.has(type)) {
      throw new Error(`${type.name} already has a handler`);
    }
    this.handlers.set(type, handler);
  }

  async execute<C extends Command<unknown>>(command: C): Promise<ResultOf<C>> {
    const handler = this.handlers.get(
      command.constructor as CommandClass<Command<unknown>>,
    );
    if (!handler) {
      throw new Error(`No handler for ${command.constructor.name}`);
    }
    return handler.execute(command) as Promise<ResultOf<C>>;
  }
}
