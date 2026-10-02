import {
  formatTimestamp,
  levelStyle,
  metricDisplayData,
  metricTimestamp,
} from "../log-format";

const local = (ms: number) => formatTimestamp(new Date(ms));

describe("formatTimestamp", () => {
  it("formats local month-day and zero-padded time", () => {
    expect(formatTimestamp(new Date(2024, 0, 5, 3, 4, 9))).toEqual({
      date: "01-05",
      time: "03:04:09",
    });
  });

  it("returns null for missing or invalid dates", () => {
    expect(formatTimestamp(null)).toBeNull();
    expect(formatTimestamp(new Date("nope"))).toBeNull();
  });
});

describe("metricTimestamp", () => {
  it("prefers the parsed event timestamp", () => {
    const parsed = new Date(2024, 0, 5, 3, 4, 9);
    expect(metricTimestamp(parsed, { timestamp: 0 })).toEqual(
      formatTimestamp(parsed),
    );
  });

  it("treats small numbers as epoch seconds and large ones as milliseconds", () => {
    expect(metricTimestamp(null, { timestamp: 1704103200 })).toEqual(
      local(1704103200 * 1000),
    );
    expect(metricTimestamp(null, { timestamp: 1704103200123 })).toEqual(
      local(1704103200123),
    );
  });

  it("parses digit strings (optionally Z-suffixed) and date strings", () => {
    expect(metricTimestamp(null, { timestamp: "1704103200Z" })).toEqual(
      local(1704103200 * 1000),
    );
    expect(
      metricTimestamp(null, { timestamp: "2024-01-01T10:00:00Z" }),
    ).toEqual(local(Date.parse("2024-01-01T10:00:00Z")));
  });

  it("returns null when there is nothing usable", () => {
    expect(metricTimestamp(null, {})).toBeNull();
    expect(metricTimestamp(null, { timestamp: { at: 1 } })).toBeNull();
    expect(metricTimestamp(null, { timestamp: "not a date" })).toBeNull();
  });
});

describe("metricDisplayData", () => {
  it("drops the marker and timestamp and stringifies values", () => {
    expect(
      metricDisplayData({
        _metric: "price",
        timestamp: 1,
        value: 2.5,
        tags: { a: 1 },
        ok: true,
      }),
    ).toEqual([
      { key: "value", value: "2.5" },
      { key: "tags", value: '{"a":1}' },
      { key: "ok", value: "true" },
    ]);
  });
});

describe("levelStyle", () => {
  it("maps known levels and falls back to the INFO style", () => {
    expect(levelStyle("ERROR")).toContain("text-red-600");
    expect(levelStyle("WARN")).toContain("text-yellow-700");
    expect(levelStyle("DEBUG")).toContain("text-gray-600");
    expect(levelStyle("TRACE")).toBe(levelStyle("INFO"));
    expect(levelStyle("constructor")).toBe(levelStyle("INFO"));
  });
});
