import { readFileSync } from "fs";
import * as path from "path";

/** The repository's contracts/ folder, shared with the runtime's tests. */
const CONTRACTS_DIR = path.resolve(__dirname, "../../../contracts");

/** Reads a file of the shared contracts, e.g. `events/bot-created.json`. */
export function loadContract<T>(name: string): T {
  return JSON.parse(readFileSync(path.join(CONTRACTS_DIR, name), "utf8"));
}
