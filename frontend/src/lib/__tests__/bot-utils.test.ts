import {
  shouldUseLogStreaming,
  isScheduledBot,
  isBotEnabled,
} from "@/lib/bot-utils";
import { Bot } from "@/lib/api/api-client";

function makeBot(config: Record<string, any> = {}): Bot {
  return {
    id: "test-bot-id",
    config,
    createdAt: "2024-01-01T00:00:00Z",
    updatedAt: "2024-01-01T00:00:00Z",
  };
}

describe("shouldUseLogStreaming", () => {
  it("returns false when bot is null", () => {
    expect(shouldUseLogStreaming(null)).toBe(false);
  });

  it("handles undefined gracefully at runtime (beyond type contract)", () => {
    expect(shouldUseLogStreaming(undefined as any)).toBe(false);
  });

  // Every loaded bot streams: scheduled runs flow through the same
  // NATS -> SSE pipeline as realtime bots, so their dashboards and consoles
  // get run output pushed live instead of waiting on a polling tick.
  it("returns true for realtime bots", () => {
    expect(shouldUseLogStreaming(makeBot({ type: "realtime/my-bot" }))).toBe(
      true,
    );
  });

  it("returns true for scheduled bots", () => {
    expect(shouldUseLogStreaming(makeBot({ type: "scheduled/my-bot" }))).toBe(
      true,
    );
  });

  it("returns true for event bots", () => {
    expect(shouldUseLogStreaming(makeBot({ type: "event/my-bot" }))).toBe(true);
  });

  it("returns true when config.type is undefined", () => {
    expect(shouldUseLogStreaming(makeBot({}))).toBe(true);
  });

  it("returns true when config.type is not a string", () => {
    expect(shouldUseLogStreaming(makeBot({ type: 42 }))).toBe(true);
  });
});

describe("isScheduledBot", () => {
  it("returns false when bot is null", () => {
    expect(isScheduledBot(null)).toBe(false);
  });

  it("returns true for scheduled bots", () => {
    expect(isScheduledBot(makeBot({ type: "scheduled/daily-trader" }))).toBe(
      true,
    );
  });

  it("returns false for realtime bots", () => {
    expect(isScheduledBot(makeBot({ type: "realtime/my-bot" }))).toBe(false);
  });

  it("returns false when config.type is undefined", () => {
    expect(isScheduledBot(makeBot({}))).toBe(false);
  });

  it("returns false when config.type is not a string", () => {
    expect(isScheduledBot(makeBot({ type: 42 }))).toBe(false);
  });
});

// The runtime runs a bot unless config.enabled is false (runtime/internal/desired),
// so the dashboard shows exactly those bots as enabled.
describe("isBotEnabled", () => {
  it("is false when config.enabled is false", () => {
    expect(isBotEnabled({ enabled: false })).toBe(false);
  });

  it("is true when config.enabled is true", () => {
    expect(isBotEnabled({ enabled: true })).toBe(true);
  });

  it("is true when config has no enabled", () => {
    expect(isBotEnabled({ symbol: "BTC/USD" })).toBe(true);
  });

  it("is true when there is no config", () => {
    expect(isBotEnabled(undefined)).toBe(true);
    expect(isBotEnabled(null)).toBe(true);
  });

  it("is true when config.enabled is anything but false", () => {
    for (const enabled of [null, 0, "", "false"]) {
      expect(isBotEnabled({ enabled })).toBe(true);
    }
  });
});
