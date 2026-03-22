import { renderHook, act } from "@testing-library/react";
import { useBotFilters } from "../use-bot-filters";
import { Bot } from "@/lib/api/api-client";

const makeBots = (): Bot[] => [
  {
    id: "bot-1",
    config: {
      name: "Alpha Bot",
      symbol: "BTCUSD",
      schedule: "0 * * * *",
      enabled: true,
    },
    createdAt: "2024-01-01T00:00:00Z",
    updatedAt: "2024-01-01T00:00:00Z",
  },
  {
    id: "bot-2",
    config: { name: "Beta Bot", symbol: "ETHUSD", enabled: true },
    createdAt: "2024-01-01T00:00:00Z",
    updatedAt: "2024-01-01T00:00:00Z",
  },
  {
    id: "bot-3",
    config: {
      name: "Gamma Bot",
      symbol: "SOLUSD",
      schedule: "0 0 * * *",
      enabled: false,
    },
    createdAt: "2024-01-01T00:00:00Z",
    updatedAt: "2024-01-01T00:00:00Z",
  },
  {
    id: "bot-4",
    config: { name: "Delta Bot", symbol: "ADAUSD", enabled: false },
    createdAt: "2024-01-01T00:00:00Z",
    updatedAt: "2024-01-01T00:00:00Z",
  },
];

describe("useBotFilters", () => {
  it("returns all bots when no filters are active", () => {
    const { result } = renderHook(() => useBotFilters());
    const bots = makeBots();

    expect(result.current.filterBots(bots)).toHaveLength(4);
    expect(result.current.hasActiveFilters).toBe(false);
  });

  it("filters by name (case-insensitive)", () => {
    const { result } = renderHook(() => useBotFilters());
    const bots = makeBots();

    act(() => result.current.setSearch("alpha"));
    expect(result.current.filterBots(bots)).toHaveLength(1);
    expect(result.current.filterBots(bots)[0].id).toBe("bot-1");
  });

  it("filters by symbol (case-insensitive)", () => {
    const { result } = renderHook(() => useBotFilters());
    const bots = makeBots();

    act(() => result.current.setSearch("ethusd"));
    expect(result.current.filterBots(bots)).toHaveLength(1);
    expect(result.current.filterBots(bots)[0].id).toBe("bot-2");
  });

  it("type filter 'scheduled' returns only bots with schedule", () => {
    const { result } = renderHook(() => useBotFilters());
    const bots = makeBots();

    act(() => result.current.setType("scheduled"));
    const filtered = result.current.filterBots(bots);
    expect(filtered).toHaveLength(2);
    expect(filtered.map((b) => b.id)).toEqual(["bot-1", "bot-3"]);
  });

  it("type filter 'realtime' returns only bots without schedule", () => {
    const { result } = renderHook(() => useBotFilters());
    const bots = makeBots();

    act(() => result.current.setType("realtime"));
    const filtered = result.current.filterBots(bots);
    expect(filtered).toHaveLength(2);
    expect(filtered.map((b) => b.id)).toEqual(["bot-2", "bot-4"]);
  });

  it("status filter 'enabled' returns only enabled bots", () => {
    const { result } = renderHook(() => useBotFilters());
    const bots = makeBots();

    act(() => result.current.setStatus("enabled"));
    const filtered = result.current.filterBots(bots);
    expect(filtered).toHaveLength(2);
    expect(filtered.map((b) => b.id)).toEqual(["bot-1", "bot-2"]);
  });

  it("status filter 'disabled' returns only disabled bots", () => {
    const { result } = renderHook(() => useBotFilters());
    const bots = makeBots();

    act(() => result.current.setStatus("disabled"));
    const filtered = result.current.filterBots(bots);
    expect(filtered).toHaveLength(2);
    expect(filtered.map((b) => b.id)).toEqual(["bot-3", "bot-4"]);
  });

  it("combines text search + type + status (AND logic)", () => {
    const { result } = renderHook(() => useBotFilters());
    const bots = makeBots();

    act(() => {
      result.current.setSearch("bot");
      result.current.setType("scheduled");
      result.current.setStatus("enabled");
    });

    const filtered = result.current.filterBots(bots);
    expect(filtered).toHaveLength(1);
    expect(filtered[0].id).toBe("bot-1");
  });

  it("toggling same type value resets to 'all'", () => {
    const { result } = renderHook(() => useBotFilters());

    act(() => result.current.setType("scheduled"));
    expect(result.current.type).toBe("scheduled");

    act(() => result.current.setType("scheduled"));
    expect(result.current.type).toBe("all");
  });

  it("toggling same status value resets to 'all'", () => {
    const { result } = renderHook(() => useBotFilters());

    act(() => result.current.setStatus("enabled"));
    expect(result.current.status).toBe("enabled");

    act(() => result.current.setStatus("enabled"));
    expect(result.current.status).toBe("all");
  });

  it("clearing search shows all bots again", () => {
    const { result } = renderHook(() => useBotFilters());
    const bots = makeBots();

    act(() => result.current.setSearch("alpha"));
    expect(result.current.filterBots(bots)).toHaveLength(1);

    act(() => result.current.setSearch(""));
    expect(result.current.filterBots(bots)).toHaveLength(4);
  });

  it("activeCount reflects number of non-text filters", () => {
    const { result } = renderHook(() => useBotFilters());

    expect(result.current.activeCount).toBe(0);

    act(() => result.current.setType("scheduled"));
    expect(result.current.activeCount).toBe(1);

    act(() => result.current.setStatus("enabled"));
    expect(result.current.activeCount).toBe(2);

    act(() => result.current.setType("scheduled")); // toggle off
    expect(result.current.activeCount).toBe(1);

    // Text search does not count toward activeCount
    act(() => result.current.setSearch("test"));
    expect(result.current.activeCount).toBe(1);
  });

  it("hasActiveFilters is true when any filter is set", () => {
    const { result } = renderHook(() => useBotFilters());

    expect(result.current.hasActiveFilters).toBe(false);

    act(() => result.current.setSearch("test"));
    expect(result.current.hasActiveFilters).toBe(true);

    act(() => result.current.setSearch(""));
    expect(result.current.hasActiveFilters).toBe(false);

    act(() => result.current.setType("scheduled"));
    expect(result.current.hasActiveFilters).toBe(true);
  });
});
