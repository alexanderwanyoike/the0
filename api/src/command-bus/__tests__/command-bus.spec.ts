import { CommandBus } from "../command-bus";
import { Command, CommandHandler } from "../command";

class RenameThing extends Command<string> {
  constructor(readonly name: string) {
    super();
  }
}

class DeleteThing extends Command<boolean> {
  constructor(readonly id: string) {
    super();
  }
}

class RenameThingHandler implements CommandHandler<RenameThing> {
  received: RenameThing[] = [];

  async execute(command: RenameThing): Promise<string> {
    this.received.push(command);
    return `renamed to ${command.name}`;
  }
}

describe("CommandBus", () => {
  let bus: CommandBus;

  beforeEach(() => {
    bus = new CommandBus();
  });

  describe("route each command to its handler", () => {
    it("hands the command to the handler registered for its class and returns its result", async () => {
      const handler = new RenameThingHandler();
      bus.register(RenameThing, handler);
      const command = new RenameThing("Momentum");

      const result = await bus.execute(command);

      expect(result).toBe("renamed to Momentum");
      expect(handler.received).toEqual([command]);
    });

    it("keeps commands of different classes apart", async () => {
      const rename = new RenameThingHandler();
      const remove = { execute: jest.fn().mockResolvedValue(true) };
      bus.register(RenameThing, rename);
      bus.register(DeleteThing, remove);

      await bus.execute(new DeleteThing("thing-1"));

      expect(remove.execute).toHaveBeenCalledWith(new DeleteThing("thing-1"));
      expect(rename.received).toEqual([]);
    });
  });

  describe("refuse a command it cannot route", () => {
    it("rejects a command no handler is registered for", async () => {
      await expect(bus.execute(new DeleteThing("thing-1"))).rejects.toThrow(
        "No handler for DeleteThing",
      );
    });

    it("refuses a second handler for the same command", () => {
      bus.register(RenameThing, new RenameThingHandler());

      expect(() => bus.register(RenameThing, new RenameThingHandler())).toThrow(
        "RenameThing already has a handler",
      );
    });
  });
});
