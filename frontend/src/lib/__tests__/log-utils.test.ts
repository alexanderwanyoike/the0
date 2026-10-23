import {
  MAX_LOG_ENTRIES,
  buildLogsSearchParams,
  filterLiveEntries,
  formatLogsForExport,
  isHistoricalQuery,
  mergeUniqueEntries,
  parseLiveUpdate,
} from "../log-utils";
import type { LogEntry } from "@/types/logs";

const entry = (content: string, key = content): LogEntry => ({
  key,
  date: "20240101",
  timestamp: null,
  content,
});

const liveEntry = (index: number, content: string): LogEntry => ({
  key: `1792713599999000000-${index}`,
  date: "20261022",
  timestamp: "2026-10-22T23:59:59.999Z",
  content,
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

  it("includes the cursor, type and sort only when set", () => {
    const params = buildLogsSearchParams({
      date: "20240101",
      limit: 50,
      type: "metrics",
      sort: "asc",
    });
    expect(params.get("limit")).toBe("50");
    expect(params.has("before")).toBe(false);
    expect(params.get("type")).toBe("metrics");
    expect(params.get("sort")).toBe("asc");
    expect(
      buildLogsSearchParams({ date: "20240101", before: "20240101:20" }).get(
        "before",
      ),
    ).toBe("20240101:20");
  });
});

describe("mergeUniqueEntries", () => {
  it("appends only entries whose key is not already held", () => {
    const merged = mergeUniqueEntries(
      [entry("a", "k1"), entry("b", "k2")],
      [entry("b", "k2"), entry("c", "k3")],
      "append",
    );
    expect(merged.map((e) => e.key)).toEqual(["k1", "k2", "k3"]);
  });

  it("keeps distinct lines that share a day and content", () => {
    const merged = mergeUniqueEntries(
      [entry("heartbeat", "1792713599999000000-0")],
      [entry("heartbeat", "1792713600999000000-0")],
      "append",
    );
    expect(merged.map((e) => e.key)).toEqual([
      "1792713599999000000-0",
      "1792713600999000000-0",
    ]);
  });

  it("prepends unique entries ahead of existing ones", () => {
    const merged = mergeUniqueEntries(
      [entry("b", "k2")],
      [entry("a", "k1"), entry("b", "k2")],
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
  it("returns the entries of an update as sent", () => {
    const entries = [
      liveEntry(0, '{"_key":"1792713599999000000-0","message":"one"}'),
      liveEntry(1, '{"_key":"1792713599999000000-1","message":"two"}'),
    ];
    expect(parseLiveUpdate(JSON.stringify(entries))).toEqual(entries);
  });

  it("throws on payloads that are not JSON", () => {
    expect(() => parseLiveUpdate("{nope")).toThrow(SyntaxError);
  });

  it("throws on payloads that are not a list of entries", () => {
    expect(() =>
      parseLiveUpdate(JSON.stringify({ content: "a", timestamp: "t" })),
    ).toThrow();
  });
});

describe("filterLiveEntries", () => {
  it("keeps only metric lines for a metrics query, by the api's rule", () => {
    const metric = liveEntry(0, '{"_metric":"price","v":1}');
    const entries = [
      metric,
      liveEntry(1, '{"message":"mentions _metric in passing"}'),
      liveEntry(2, '{"_metric":"","v":1}'),
      liveEntry(3, "null"),
      liveEntry(4, "{bad json"),
    ];
    expect(filterLiveEntries(entries, "metrics")).toEqual([metric]);
  });

  it("keeps every line for other queries", () => {
    const entries = [liveEntry(0, "plain"), liveEntry(1, '{"_metric":"a"}')];
    expect(filterLiveEntries(entries)).toEqual(entries);
  });
});

describe("formatLogsForExport", () => {
  it("prefixes each line with its date", () => {
    expect(
      formatLogsForExport([entry("a"), { ...entry("b"), date: "20240102" }]),
    ).toBe("[20240101] a\n[20240102] b");
  });
});
