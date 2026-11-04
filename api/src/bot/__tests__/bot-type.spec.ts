import { appError } from "@/common/app-error";
import { Failure, Ok } from "@/common/result";
import { parseBotType } from "../bot-type";

describe("parseBotType", () => {
  it.each([
    ["scheduled/sma-bot", { kind: "scheduled", customBotName: "sma-bot" }],
    ["realtime/grid-2", { kind: "realtime", customBotName: "grid-2" }],
  ])("reads %s as a Bot kind and a Custom bot name", (type, parsed) => {
    expect(parseBotType(type)).toEqual(Ok(parsed));
  });

  it.each([
    "",
    "sma-bot",
    "scheduled/",
    "scheduled/ ",
    "/sma-bot",
    "scheduled/sma-bot/extra",
    "Scheduled/sma-bot",
    "scheduled/sma_bot",
  ])("refuses %j", (type) => {
    expect(parseBotType(type)).toEqual(
      Failure(
        appError(
          "validation",
          "Invalid bot type format. Expected format: type/name",
        ),
      ),
    );
  });
});
