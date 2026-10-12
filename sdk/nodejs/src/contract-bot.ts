/**
 * The contract bot: runs one sdk/contract case against this SDK.
 *
 * Usage: node contract-bot.js <case.json> <query result path>
 *
 * contract.test.ts compiles this file with the SDK sources and starts it with
 * the case's environment. Each op makes the SDK call sdk/contract/README.md
 * maps it to.
 */

import * as fs from "fs";
import * as the0 from "./index";
import { query, state } from "./index";

type Json = unknown;
type Step = { op: string; as?: string; catch?: string; [arg: string]: Json };
type Bound = Record<string, Json>;

interface ContractCase {
  entrypoint: "bot" | "query";
  script?: Step[];
  handlers?: Record<string, Step[]>;
}

const catchable: Record<string, (e: unknown) => boolean> = {
  read_only_state: (e) => e instanceof query.ReadOnlyStateError,
  any: () => true,
};

function resolve(value: Json, bound: Bound): Json {
  if (Array.isArray(value)) {
    return value.map((item) => resolve(item, bound));
  }
  if (value !== null && typeof value === "object") {
    const entries = Object.entries(value as Record<string, Json>);
    if (entries.length === 1 && entries[0][0] === "$ref") {
      return bound[entries[0][1] as string];
    }
    return Object.fromEntries(entries.map(([key, item]) => [key, resolve(item, bound)]));
  }
  return value;
}

type Args = Record<string, any>;

/** Calls fn with the optional args the step gives, so an omitted one stays omitted. */
function callWith(fn: (...values: any[]) => unknown, args: Args, required: string[], optional: string[] = []): unknown {
  const given = optional.filter((name) => name in args);
  return fn(...[...required, ...given].map((name) => args[name]));
}

async function perform(step: Step, request: query.QueryRequest | null, bound: Bound): Promise<Json> {
  const args: Args = {};
  for (const [key, value] of Object.entries(step)) {
    if (key !== "op" && key !== "as" && key !== "catch") {
      args[key] = resolve(value, bound);
    }
  }

  switch (step.op) {
    case "parse": {
      const { id, config } = the0.parse();
      return { bot_id: id, config };
    }
    case "success":
      return callWith(the0.success, args, ["message"], ["data"]);
    case "error":
      return callWith(the0.error, args, ["message"], ["data"]);
    case "result":
      return the0.result(args.value);
    case "metric":
      return the0.metric(args.type, args.data);
    case "log":
      return callWith(the0.log, args, ["message"], ["data", "level"]);
    case "raise":
      throw new Error(args.message);

    case "state.get":
      return callWith(state.get, args, ["key"], ["default"]);
    case "state.set":
      return state.set(args.key, args.value);
    case "state.delete":
      return state.delete(args.key);
    case "state.list":
      return state.list();
    case "state.exists":
      return state.exists(args.key);
    case "state.clear":
      return state.clear();
  }

  if (request === null) {
    throw new Error(`contract-bot: op ${step.op} is only valid inside a query handler`);
  }
  switch (step.op) {
    case "query.params":
      return { ...request.params };
    case "query.param":
      return callWith(request.get, args, ["key"], ["default"]);
    case "query.config":
      return query.getConfig();
  }
  throw new Error(`contract-bot: unknown op ${step.op}`);
}

async function runSteps(steps: Step[], request: query.QueryRequest | null = null): Promise<Json> {
  const bound: Bound = {};
  for (const step of steps) {
    if (step.op === "return") {
      return resolve(step.value, bound);
    }
    let value: Json;
    try {
      value = await perform(step, request, bound);
    } catch (e) {
      if (step.catch === undefined || !catchable[step.catch]?.(e)) {
        throw e;
      }
      value = { caught: step.catch };
    }
    if (step.as !== undefined) {
      bound[step.as] = value === undefined ? null : value;
    }
  }
  return null;
}

async function main(): Promise<void> {
  const [casePath, queryResultPath] = process.argv.slice(2);
  const contractCase = JSON.parse(fs.readFileSync(casePath, "utf8")) as ContractCase;

  query._queryResultFile.path = queryResultPath;

  if (contractCase.entrypoint === "bot") {
    await runSteps(contractCase.script ?? []);
    return;
  }

  for (const [path, steps] of Object.entries(contractCase.handlers ?? {})) {
    query.handler(path, (request) => runSteps(steps, request));
  }
  await query.run();
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
