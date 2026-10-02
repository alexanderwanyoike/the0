import React from "react";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import DashboardPage from "../page";
import type { Bot } from "@/lib/api/api-client";

const mockRouter = { push: jest.fn(), replace: jest.fn() };
jest.mock("next/navigation", () => ({
  useRouter: () => mockRouter,
  usePathname: () => "/dashboard",
}));

let mockBotsState: {
  bots: Bot[];
  loading: boolean;
  error: string | null;
  refetchBots: jest.Mock;
};
jest.mock("@/contexts/dashboard-bots-context", () => ({
  useDashboardBots: () => mockBotsState,
}));

const scheduledBot: Bot = {
  id: "bot-sched",
  config: {
    name: "SMA Crossover",
    symbol: "AAPL",
    type: "scheduled/sma",
    schedule: "0 9 * * 1",
    enabled: true,
  },
  createdAt: "2024-01-01T00:00:00Z",
  updatedAt: "2024-01-01T00:00:00Z",
};

const realtimeBot: Bot = {
  id: "bot-rt",
  config: {
    name: "Momentum Scalper",
    symbol: "BTCUSD",
    type: "realtime/momentum",
    enabled: false,
  },
  createdAt: "2024-01-01T00:00:00Z",
  updatedAt: "2024-01-01T00:00:00Z",
};

const unnamedBot: Bot = {
  id: "bot-unnamed",
  config: { schedule: "every day" },
  createdAt: "2024-01-01T00:00:00Z",
  updatedAt: "2024-01-01T00:00:00Z",
};

function setViewport(viewport: "desktop" | "mobile") {
  (window.matchMedia as jest.Mock).mockImplementation((query: string) => ({
    matches: viewport === "desktop",
    media: query,
    onchange: null,
    addListener: jest.fn(),
    removeListener: jest.fn(),
    addEventListener: jest.fn(),
    removeEventListener: jest.fn(),
    dispatchEvent: jest.fn(),
  }));
}

function setBots(overrides: Partial<typeof mockBotsState>) {
  mockBotsState = { ...mockBotsState, ...overrides };
}

beforeEach(() => {
  mockRouter.push.mockClear();
  mockRouter.replace.mockClear();
  mockBotsState = {
    bots: [scheduledBot, realtimeBot, unnamedBot],
    loading: false,
    error: null,
    refetchBots: jest.fn(),
  };
  setViewport("mobile");
});

describe("DashboardPage", () => {
  describe.each(["desktop", "mobile"] as const)(
    "load states on %s",
    (viewport) => {
      beforeEach(() => setViewport(viewport));

      it("shows a spinner while the bots load", () => {
        setBots({ loading: true });
        const { container } = render(<DashboardPage />);

        expect(container.querySelector(".animate-spin")).toBeInTheDocument();
        expect(screen.queryByText("Trading Bots")).not.toBeInTheDocument();
        expect(mockRouter.replace).not.toHaveBeenCalled();
      });

      it("shows the error with a retry that refetches the bots", async () => {
        setBots({ error: "Network down" });
        render(<DashboardPage />);

        expect(screen.getByText("Failed to load bots")).toBeInTheDocument();
        expect(screen.getByText("Network down")).toBeInTheDocument();

        await userEvent.click(
          screen.getByRole("button", { name: /try again/i }),
        );
        expect(mockBotsState.refetchBots).toHaveBeenCalledTimes(1);
      });

      it("shows the empty state with links to other bot pages", () => {
        setBots({ bots: [] });
        render(<DashboardPage />);

        expect(screen.getByText("No trading bots yet")).toBeInTheDocument();
        expect(
          screen.getByRole("link", { name: /view my bots/i }),
        ).toHaveAttribute("href", "/user-bots");
        expect(
          screen.getByRole("link", { name: /custom bots/i }),
        ).toHaveAttribute("href", "/custom-bots");
        expect(mockRouter.replace).not.toHaveBeenCalled();
      });
    },
  );

  describe("on desktop", () => {
    beforeEach(() => setViewport("desktop"));

    it("redirects to the first bot and renders nothing itself", () => {
      const { container } = render(<DashboardPage />);

      expect(mockRouter.replace).toHaveBeenCalledWith("/dashboard/bot-sched");
      expect(container).toBeEmptyDOMElement();
    });
  });

  describe("mobile list", () => {
    it("shows the title and the bot count", () => {
      render(<DashboardPage />);

      expect(
        screen.getByRole("heading", { name: "Trading Bots" }),
      ).toBeInTheDocument();
      expect(screen.getByText("3 bots")).toBeInTheDocument();
    });

    it("uses the singular for a single bot", () => {
      setBots({ bots: [scheduledBot] });
      render(<DashboardPage />);

      expect(screen.getByText("1 bot")).toBeInTheDocument();
    });

    it("renders each bot with its symbol, type and schedule", () => {
      render(<DashboardPage />);

      const scheduled = screen.getByRole("button", { name: /SMA Crossover/ });
      expect(scheduled).toHaveTextContent("AAPL");
      expect(scheduled).toHaveTextContent("scheduled/sma");
      expect(scheduled).toHaveTextContent("At 09:00 AM, only on Monday");
      expect(scheduled.querySelector(".bg-green-500")).toBeInTheDocument();

      const realtime = screen.getByRole("button", { name: /Momentum Scalper/ });
      expect(realtime).toHaveTextContent("BTCUSD");
      expect(realtime).toHaveTextContent("Real-time");
      expect(realtime.querySelector(".bg-gray-400")).toBeInTheDocument();
    });

    it("falls back to the bot id, a generic type and the raw schedule", () => {
      render(<DashboardPage />);

      const unnamed = screen.getByRole("button", { name: /bot-unnamed/ });
      expect(unnamed).toHaveTextContent("Bot");
      expect(unnamed).toHaveTextContent("every day");
    });

    it("opens a bot when it is selected and never redirects", async () => {
      render(<DashboardPage />);

      await userEvent.click(
        screen.getByRole("button", { name: /Momentum Scalper/ }),
      );

      expect(mockRouter.push).toHaveBeenCalledWith("/dashboard/bot-rt");
      expect(mockRouter.replace).not.toHaveBeenCalled();
    });
  });

  describe("mobile filtering", () => {
    it("narrows the list by name and shows filtered over total", async () => {
      render(<DashboardPage />);

      await userEvent.type(
        screen.getByRole("textbox", { name: "Filter bots" }),
        "momentum",
      );

      expect(screen.getByText("1 / 3 bots")).toBeInTheDocument();
      expect(
        screen.getByRole("button", { name: /Momentum Scalper/ }),
      ).toBeInTheDocument();
      expect(
        screen.queryByRole("button", { name: /SMA Crossover/ }),
      ).not.toBeInTheDocument();
    });

    it("matches on the symbol", async () => {
      render(<DashboardPage />);

      await userEvent.type(
        screen.getByRole("textbox", { name: "Filter bots" }),
        "aapl",
      );

      expect(
        screen.getByRole("button", { name: /SMA Crossover/ }),
      ).toBeInTheDocument();
      expect(screen.getByText("1 / 3 bots")).toBeInTheDocument();
    });

    it("says so when nothing matches", async () => {
      render(<DashboardPage />);

      await userEvent.type(
        screen.getByRole("textbox", { name: "Filter bots" }),
        "zzz",
      );

      expect(screen.getByText("No matching bots")).toBeInTheDocument();
      expect(screen.getByText("0 / 3 bots")).toBeInTheDocument();
    });

    it("offers type and status filters", async () => {
      render(<DashboardPage />);

      await userEvent.click(
        screen.getByRole("button", { name: "Filter bots" }),
      );

      expect(screen.getByText("Type")).toBeInTheDocument();
      expect(screen.getByText("Status")).toBeInTheDocument();
      expect(
        screen.getAllByRole("menuitemradio").map((item) => item.textContent),
      ).toEqual([
        "All",
        "Scheduled",
        "Real-time",
        "All",
        "Enabled",
        "Disabled",
      ]);
    });

    it("filters by type and counts the active filter", async () => {
      render(<DashboardPage />);

      await userEvent.click(
        screen.getByRole("button", { name: "Filter bots" }),
      );
      await userEvent.click(
        screen.getByRole("menuitemradio", { name: "Real-time" }),
      );

      expect(screen.getByText("1 / 3 bots")).toBeInTheDocument();
      expect(
        screen.getByRole("button", { name: /Momentum Scalper/ }),
      ).toBeInTheDocument();
      expect(
        screen.getByRole("button", { name: "Filter bots (1 active)" }),
      ).toHaveTextContent("1");
    });

    it("filters by status", async () => {
      render(<DashboardPage />);

      await userEvent.click(
        screen.getByRole("button", { name: "Filter bots" }),
      );
      await userEvent.click(
        screen.getByRole("menuitemradio", { name: "Enabled" }),
      );

      expect(screen.getByText("2 / 3 bots")).toBeInTheDocument();
      expect(
        screen.queryByRole("button", { name: /Momentum Scalper/ }),
      ).not.toBeInTheDocument();
    });

    it("clears a filter when its option is picked again", async () => {
      render(<DashboardPage />);
      const openMenu = () =>
        userEvent.click(screen.getByRole("button", { name: /^Filter bots/ }));

      await openMenu();
      await userEvent.click(
        screen.getByRole("menuitemradio", { name: "Scheduled" }),
      );
      expect(screen.getByText("2 / 3 bots")).toBeInTheDocument();

      await openMenu();
      await userEvent.click(
        screen.getByRole("menuitemradio", { name: "Scheduled" }),
      );
      expect(screen.getByText("3 bots")).toBeInTheDocument();
      expect(
        screen.getByRole("button", { name: "Filter bots" }),
      ).toBeInTheDocument();
    });
  });
});
