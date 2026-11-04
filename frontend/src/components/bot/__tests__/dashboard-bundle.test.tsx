import { execFileSync } from "node:child_process";
import path from "node:path";
import type { ComponentType } from "react";
import { render, screen } from "@testing-library/react";
// Imported for its side effect: it exposes the host's React on window, as
// it does before any bundle loads.
import "@/components/bot/bot-dashboard-loader";
import { BotEventsProvider } from "@/contexts/bot-events-context";
import type { LogEntry } from "@/types/logs";

const mockLogs: LogEntry[] = [];

jest.mock("@/hooks/use-bot-logs", () => ({
  ...jest.requireActual("@/hooks/use-bot-logs"),
  useBotLogs: () => ({
    logs: mockLogs,
    loading: false,
    isFetching: false,
    connected: undefined,
    lastUpdate: null,
    error: null,
    refresh: jest.fn(),
    setDateFilter: jest.fn(),
    setDateRangeFilter: jest.fn(),
    setLatestFilter: jest.fn(),
    exportLogs: jest.fn(),
  }),
}));

const EXAMPLE = path.resolve(
  __dirname,
  "../../../../../example-bots/typescript-price-alerts/frontend/index.tsx",
);
const BUILD_SCRIPT = path.resolve(
  __dirname,
  "../../../test/build-dashboard-bundles.mjs",
);

let bundles: Record<"published" | "source", string>;

beforeAll(() => {
  bundles = JSON.parse(
    execFileSync(process.execPath, [BUILD_SCRIPT, EXAMPLE], {
      encoding: "utf8",
    }),
  );
}, 60_000);

/** Evaluates a bundle the way the loader's import() would, minus the ESM wrapper. */
function loadDashboard(code: string): ComponentType {
  const cjs = { exports: {} as { default?: ComponentType } };
  new Function("module", "exports", code)(cjs, cjs.exports);
  if (!cjs.exports.default) throw new Error("bundle has no default export");
  return cjs.exports.default;
}

function metricLine(metric: Record<string, unknown>, at: Date): LogEntry {
  const content = JSON.stringify(metric);
  return {
    key: `${at.toISOString()}-${content}`,
    date: at.toISOString().slice(0, 10).replace(/-/g, ""),
    timestamp: at.toISOString(),
    content,
  };
}

describe.each(["published", "source"] as const)(
  "the price-alerts example dashboard built with the %s the0-react",
  (sdk) => {
    beforeEach(() => {
      const now = new Date();
      mockLogs.splice(
        0,
        mockLogs.length,
        metricLine(
          {
            _metric: "price",
            symbol: "BTC/USD",
            value: 67250,
            change_pct: 1.25,
            high_24h: 68000,
            low_24h: 66000,
          },
          now,
        ),
        metricLine(
          {
            _metric: "alert",
            symbol: "BTC/USD",
            type: "price_spike",
            change_pct: 1.25,
            message: "BTC/USD up 1.25% in 5 minutes",
            severity: "high",
          },
          now,
        ),
      );
    });

    it("renders the host's events through the host's React", () => {
      const Dashboard = loadDashboard(bundles[sdk]);

      render(
        <BotEventsProvider botId="bot-1">
          <Dashboard />
        </BotEventsProvider>,
      );

      expect(screen.getByText("Price Alerts")).toBeInTheDocument();
      expect(
        screen.getByText(
          (_, element) =>
            element?.textContent === "1 price updates, 1 alerts, 0 signals",
        ),
      ).toBeInTheDocument();
      expect(screen.getAllByText("$67,250").length).toBeGreaterThan(0);
      expect(
        screen.getByText("BTC/USD up 1.25% in 5 minutes"),
      ).toBeInTheDocument();
    });
  },
);
