import { shouldUseLogStreaming } from "@/lib/bot-utils";
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

  it("returns false when bot is undefined", () => {
    expect(shouldUseLogStreaming(undefined as any)).toBe(false);
  });

  it('returns true when config.type is "realtime/my-bot"', () => {
    expect(shouldUseLogStreaming(makeBot({ type: "realtime/my-bot" }))).toBe(
      true,
    );
  });

  it('returns true when config.type is "realtime/another-bot"', () => {
    expect(
      shouldUseLogStreaming(makeBot({ type: "realtime/another-bot" })),
    ).toBe(true);
  });

  it('returns false when config.type is "scheduled/my-bot"', () => {
    expect(shouldUseLogStreaming(makeBot({ type: "scheduled/my-bot" }))).toBe(
      false,
    );
  });

  it('returns false when config.type is "scheduled/daily-trader"', () => {
    expect(
      shouldUseLogStreaming(makeBot({ type: "scheduled/daily-trader" })),
    ).toBe(false);
  });

  it('returns true when config.type is "event/my-bot"', () => {
    expect(shouldUseLogStreaming(makeBot({ type: "event/my-bot" }))).toBe(true);
  });

  it("returns true when config is undefined", () => {
    expect(shouldUseLogStreaming({ ...makeBot(), config: undefined } as any)).toBe(
      true,
    );
  });

  it("returns true when config.type is undefined", () => {
    expect(shouldUseLogStreaming(makeBot({}))).toBe(true);
  });

  it("returns true when config.type is null", () => {
    expect(shouldUseLogStreaming(makeBot({ type: null }))).toBe(true);
  });

  it("returns true when config.type is empty string", () => {
    expect(shouldUseLogStreaming(makeBot({ type: "" }))).toBe(true);
  });

  it("returns true when config.type is not a string (number)", () => {
    expect(shouldUseLogStreaming(makeBot({ type: 42 }))).toBe(true);
  });

  it("returns true when config.type is not a string (boolean)", () => {
    expect(shouldUseLogStreaming(makeBot({ type: true }))).toBe(true);
  });

  it("returns true when config is empty object", () => {
    expect(shouldUseLogStreaming(makeBot({}))).toBe(true);
  });
});
