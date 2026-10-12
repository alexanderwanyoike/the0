import { Failure, Ok, Result } from "@/common/result";

/** The Custom bot a Bot's type names: "scheduled/sma-bot" names "sma-bot". */
export function customBotNameOf(type: string): Result<string, string> {
  const name = type.split("/")[1];
  return name?.trim()
    ? Ok(name)
    : Failure("Invalid bot type: missing name after '/'");
}
