import { Module } from "@nestjs/common";
import { CommandBus } from "./command-bus";

@Module({
  providers: [CommandBus],
  exports: [CommandBus],
})
export class CommandBusModule {}
