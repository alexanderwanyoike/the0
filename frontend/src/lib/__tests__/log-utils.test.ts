import {
  MAX_LOG_ENTRIES,
  buildLogsSearchParams,
  expandLogEntries,
  formatLogsForExport,
  isHistoricalQuery,
  mergeUniqueEntries,
  parseLiveUpdate,
  toDisplayOrder,
} from "../log-utils";
import type { LogEntry } from "@/types/logs";

const entry = (content: string, date = "2024-01-01"): LogEntry => ({
  date,
  content,
});

describe("expandLogEntries", () => {
  it("splits multi-line content into one entry per non-empty line", () => {
    expect(
      expandLogEntries([
        { date: "d", content: "a\n\n  \nb", timestamp: "t" },
        { date: "e", content: "c" },
      ]),
    ).toEqual([
      { date: "d", content: "a", timestamp: "t" },
      { date: "d", content: "b", timestamp: "t" },
      { date: "e", content: "c", timestamp: undefined },
    ]);
  });
});

describe("isHistoricalQuery", () => {
  it("is true only when a date or date range pins the view", () => {
    expect(isHistoricalQuery({ date: "20240101" })).toBe(true);
    expect(isHistoricalQuery({ dateRange: "20240101-20240102" })).toBe(true);
    expect(isHistoricalQuery({ lookbackDays: 30 })).toBe(false);
    expect(isHistoricalQuery({})).toBe(false);
  });
});

describe("buildLogsSearchParams", () => {
  it("prefers dateRange over date over lookbackDays", () => {
    const all = buildLogsSearchParams({
      dateRange: "a-b",
      date: "20240101",
      lookbackDays: 7,
    });
    expect(all.get("dateRange")).toBe("a-b");
    expect(all.has("date")).toBe(false);
    expect(all.has("lookbackDays")).toBe(false);

    const dated = buildLogsSearchParams({ date: "20240101", lookbackDays: 7 });
    expect(dated.get("date")).toBe("20240101");
    expect(dated.has("lookbackDays")).toBe(false);

    expect(buildLogsSearchParams({ lookbackDays: 7 }).get("lookbackDays")).toBe(
      "7",
    );
  });

  it("defaults to today's compact UTC date when no window is set", () => {
    const today = new Date().toISOString().slice(0, 10).replace(/-/g, "");
    expect(buildLogsSearchParams({}).get("date")).toBe(today);
  });

  it("includes paging, type and sort only when set and non-zero", () => {
    const params = buildLogsSearchParams({
      date: "20240101",
      limit: 50,
      offset: 0,
      type: "metrics",
      sort: "asc",
    });
    expect(params.get("limit")).toBe("50");
    expect(params.has("offset")).toBe(false);
    expect(params.get("type")).toBe("metrics");
    expect(params.get("sort")).toBe("asc");
    expect(
      buildLogsSearchParams({ date: "20240101", offset: 20 }).get("offset"),
    ).toBe("20");
  });
});

describe("toDisplayOrder", () => {
  const page = [entry("newest"), entry("oldest")];

  it("reverses live replace and prepend pages into chronological order", () => {
    expect(toDisplayOrder(page, true, false).map((e) => e.content)).toEqual([
      "oldest",
      "newest",
    ]);
    expect(toDisplayOrder(page, true, "prepend").map((e) => e.content)).toEqual(
      ["oldest", "newest"],
    );
  });

  it("keeps API order for appended pages and non-live views", () => {
    expect(toDisplayOrder(page, true, "append")).toBe(page);
    expect(toDisplayOrder(page, false, false)).toBe(page);
  });

  it("does not mutate the input page", () => {
    toDisplayOrder(page, true, false);
    expect(page[0].content).toBe("newest");
  });
});

describe("mergeUniqueEntries", () => {
  it("appends only entries not already present by date and content", () => {
    const merged = mergeUniqueEntries(
      [entry("a"), entry("b")],
      [entry("b"), entry("c"), entry("a", "2024-01-02")],
      "append",
    );
    expect(merged.map((e) => `${e.date}|${e.content}`)).toEqual([
      "2024-01-01|a",
      "2024-01-01|b",
      "2024-01-01|c",
      "2024-01-02|a",
    ]);
  });

  it("prepends unique entries ahead of existing ones", () => {
    const merged = mergeUniqueEntries(
      [entry("b")],
      [entry("a"), entry("b")],
      "prepend",
    );
    expect(merged.map((e) => e.content)).toEqual(["a", "b"]);
  });

  it("trims a full buffer from the side away from the new page", () => {
    const full = Array.from({ length: MAX_LOG_ENTRIES }, (_, i) =>
      entry(`old-${i}`),
    );

    const appended = mergeUniqueEntries(full, [entry("new")], "append");
    expect(appended).toHaveLength(MAX_LOG_ENTRIES);
    expect(appended[0].content).toBe("old-1");
    expect(appended.at(-1)!.content).toBe("new");

    const prepended = mergeUniqueEntries(full, [entry("earlier")], "prepend");
    expect(prepended).toHaveLength(MAX_LOG_ENTRIES);
    expect(prepended[0].content).toBe("earlier");
    expect(prepended.at(-1)!.content).toBe(`old-${MAX_LOG_ENTRIES - 2}`);
  });
});

describe("parseLiveUpdate", () => {
  const payload = (content: string) =>
    JSON.stringify({ content, timestamp: "2024-01-01T00:00:00Z" });

  it("expands the update into timestamped lines", () => {
    expect(parseLiveUpdate(payload("one\ntwo"))).toEqual([
      {
        date: "2024-01-01T00:00:00Z",
        content: "one",
        timestamp: "2024-01-01T00:00:00Z",
      },
      {
        date: "2024-01-01T00:00:00Z",
        content: "two",
        timestamp: "2024-01-01T00:00:00Z",
      },
    ]);
  });

  it("keeps only metric lines for a metrics query", () => {
    const content = [
      '{"_metric":"price","v":1}',
      "plain line",
      "{bad json",
    ].join("\n");
    expect(
      parseLiveUpdate(payload(content), "metrics").map((e) => e.content),
    ).toEqual(['{"_metric":"price","v":1}']);
  });

  it("throws on payloads that are not JSON", () => {
    expect(() => parseLiveUpdate("{nope")).toThrow(SyntaxError);
  });
});

describe("formatLogsForExport", () => {
  it("prefixes each line with its date", () => {
    expect(formatLogsForExport([entry("a"), entry("b", "d2")])).toBe(
      "[2024-01-01] a\n[d2] b",
    );
  });
});
