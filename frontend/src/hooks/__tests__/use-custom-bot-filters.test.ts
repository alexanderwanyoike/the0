import { renderHook, act } from "@testing-library/react";
import { useCustomBotFilters } from "../use-custom-bot-filters";
import { CustomBotWithVersions } from "@/types/custom-bots";

const makeBots = (): CustomBotWithVersions[] => [
  {
    id: "cb-1",
    name: "sma-crossover",
    userId: "user-1",
    latestVersion: "1.0.0",
    versions: [
      {
        id: "v1",
        version: "1.0.0",
        userId: "user-1",
        createdAt: new Date("2024-01-01"),
        status: "active",
        config: {
          name: "sma-crossover",
          version: "1.0.0",
          description: "Simple moving average crossover strategy",
          runtime: "python3.11",
          type: "scheduled",
          author: "test",
          entrypoints: { bot: "main.py" },
          schema: {},
        },
        filePath: "/bots/sma-crossover",
      },
    ],
    createdAt: new Date("2024-01-01"),
    updatedAt: new Date("2024-01-01"),
  },
  {
    id: "cb-2",
    name: "momentum-trader",
    userId: "user-1",
    latestVersion: "2.0.0",
    versions: [
      {
        id: "v2",
        version: "2.0.0",
        userId: "user-1",
        createdAt: new Date("2024-01-01"),
        status: "active",
        config: {
          name: "momentum-trader",
          version: "2.0.0",
          description: "Momentum based trading bot",
          runtime: "nodejs20",
          type: "real-time",
          author: "test",
          entrypoints: { bot: "index.js" },
          schema: {},
        },
        filePath: "/bots/momentum-trader",
      },
    ],
    createdAt: new Date("2024-01-01"),
    updatedAt: new Date("2024-01-01"),
  },
];

describe("useCustomBotFilters", () => {
  it("returns all bots when no filters are active", () => {
    const { result } = renderHook(() => useCustomBotFilters());
    const bots = makeBots();

    expect(result.current.filterBots(bots)).toHaveLength(2);
    expect(result.current.hasActiveFilters).toBe(false);
  });

  it("filters by name (case-insensitive)", () => {
    const { result } = renderHook(() => useCustomBotFilters());
    const bots = makeBots();

    act(() => result.current.setSearch("SMA"));
    const filtered = result.current.filterBots(bots);
    expect(filtered).toHaveLength(1);
    expect(filtered[0].name).toBe("sma-crossover");
  });

  it("filters by description (case-insensitive)", () => {
    const { result } = renderHook(() => useCustomBotFilters());
    const bots = makeBots();

    act(() => result.current.setSearch("momentum"));
    const filtered = result.current.filterBots(bots);
    expect(filtered).toHaveLength(1);
    expect(filtered[0].name).toBe("momentum-trader");
  });

  it("type filter 'scheduled' returns only scheduled bots", () => {
    const { result } = renderHook(() => useCustomBotFilters());
    const bots = makeBots();

    act(() => result.current.setType("scheduled"));
    const filtered = result.current.filterBots(bots);
    expect(filtered).toHaveLength(1);
    expect(filtered[0].name).toBe("sma-crossover");
  });

  it("type filter 'realtime' returns only real-time bots", () => {
    const { result } = renderHook(() => useCustomBotFilters());
    const bots = makeBots();

    act(() => result.current.setType("realtime"));
    const filtered = result.current.filterBots(bots);
    expect(filtered).toHaveLength(1);
    expect(filtered[0].name).toBe("momentum-trader");
  });

  it("combines text search + type (AND logic)", () => {
    const { result } = renderHook(() => useCustomBotFilters());
    const bots = makeBots();

    act(() => {
      result.current.setSearch("bot");
      result.current.setType("realtime");
    });

    const filtered = result.current.filterBots(bots);
    expect(filtered).toHaveLength(1);
    expect(filtered[0].name).toBe("momentum-trader");
  });

  it("toggling same type value resets to 'all'", () => {
    const { result } = renderHook(() => useCustomBotFilters());

    act(() => result.current.setType("scheduled"));
    expect(result.current.type).toBe("scheduled");

    act(() => result.current.setType("scheduled"));
    expect(result.current.type).toBe("all");
  });

  it("activeCount reflects number of non-text filters", () => {
    const { result } = renderHook(() => useCustomBotFilters());

    expect(result.current.activeCount).toBe(0);

    act(() => result.current.setType("scheduled"));
    expect(result.current.activeCount).toBe(1);

    act(() => result.current.setType("scheduled")); // toggle off
    expect(result.current.activeCount).toBe(0);

    // Text search does not count toward activeCount
    act(() => {
      result.current.setType("realtime");
      result.current.setSearch("test");
    });
    expect(result.current.activeCount).toBe(1);
  });

  it("hasActiveFilters is true when any filter is set", () => {
    const { result } = renderHook(() => useCustomBotFilters());

    expect(result.current.hasActiveFilters).toBe(false);

    act(() => result.current.setType("realtime"));
    expect(result.current.hasActiveFilters).toBe(true);

    act(() => result.current.setType("realtime")); // toggle off
    expect(result.current.hasActiveFilters).toBe(false);

    act(() => result.current.setSearch("test"));
    expect(result.current.hasActiveFilters).toBe(true);
  });
});
