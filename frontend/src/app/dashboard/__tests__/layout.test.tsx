import React from "react";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import Layout from "../layout";
import type { Bot } from "@/lib/api/api-client";

const mockRouter = { push: jest.fn(), replace: jest.fn() };
let mockPathname = "/dashboard";
jest.mock("next/navigation", () => ({
  useRouter: () => mockRouter,
  usePathname: () => mockPathname,
}));

let mockAuth: { user: { id: string } | null; loading: boolean };
jest.mock("@/contexts/auth-context", () => ({
  useAuth: () => mockAuth,
}));

jest.mock("@/components/layouts/dashboard-layout", () => ({
  __esModule: true,
  default: ({ children }: { children: React.ReactNode }) => (
    <div data-testid="app-chrome">{children}</div>
  ),
}));

let mockBots: Bot[];
jest.mock("@/contexts/dashboard-bots-context", () => ({
  DashboardBotsProvider: ({ children }: { children: React.ReactNode }) => (
    <div data-testid="dashboard-bots-provider">{children}</div>
  ),
  useDashboardBots: () => ({
    bots: mockBots,
    loading: false,
    error: null,
    refetchBots: jest.fn(),
    removeBotFromList: jest.fn(),
  }),
}));

const smaBot: Bot = {
  id: "bot-sched",
  config: { name: "SMA Crossover", symbol: "AAPL", schedule: "0 9 * * 1" },
  createdAt: "2024-01-01T00:00:00Z",
  updatedAt: "2024-01-01T00:00:00Z",
};
const momentumBot: Bot = {
  id: "bot-rt",
  config: { name: "Momentum Scalper", symbol: "BTCUSD", enabled: false },
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

function renderLayout() {
  return render(
    <Layout>
      <div data-testid="route-content" />
    </Layout>,
  );
}

beforeEach(() => {
  mockRouter.push.mockClear();
  mockRouter.replace.mockClear();
  mockPathname = "/dashboard";
  mockAuth = { user: { id: "user-1" }, loading: false };
  mockBots = [smaBot, momentumBot];
  setViewport("desktop");
  window.localStorage.clear();
});

describe("dashboard layout", () => {
  describe("auth gate", () => {
    it("renders nothing inside the app chrome while auth loads", () => {
      mockAuth = { user: null, loading: true };
      renderLayout();

      expect(screen.getByTestId("app-chrome")).toBeEmptyDOMElement();
      expect(mockRouter.replace).not.toHaveBeenCalled();
    });

    it("sends signed-out users to the login page", () => {
      mockAuth = { user: null, loading: false };
      renderLayout();

      expect(mockRouter.replace).toHaveBeenCalledWith("/login");
      expect(screen.getByTestId("app-chrome")).toBeEmptyDOMElement();
    });

    it("renders the route inside the bots provider once signed in", () => {
      renderLayout();

      const provider = screen.getByTestId("dashboard-bots-provider");
      expect(screen.getByTestId("app-chrome")).toContainElement(provider);
      expect(provider).toContainElement(screen.getByTestId("route-content"));
      expect(screen.getByTestId("route-content").closest("main")).toHaveClass(
        "h-full",
        "overflow-auto",
      );
    });
  });

  describe("desktop sidebar", () => {
    it("lists the bots under a header with the total", () => {
      renderLayout();

      expect(screen.getByText("Bots")).toBeInTheDocument();
      expect(screen.getByText("2")).toBeInTheDocument();
      expect(
        screen.getByRole("button", { name: /SMA Crossover/ }),
      ).toHaveTextContent("AAPL");
      expect(
        screen.getByRole("button", { name: /Momentum Scalper/ }),
      ).toBeInTheDocument();
    });

    it("sits in a resizable panel next to the route content", () => {
      renderLayout();

      expect(screen.getByRole("separator")).toBeInTheDocument();
      expect(screen.getByTestId("bot-list")).toContainElement(
        screen.getByText("Bots"),
      );
      expect(screen.getByTestId("dashboard-main")).toContainElement(
        screen.getByTestId("route-content"),
      );
    });

    it("marks the bot in the URL as the current one", () => {
      mockPathname = "/dashboard/bot-rt";
      renderLayout();

      expect(
        screen.getByRole("button", { name: /Momentum Scalper/ }),
      ).toHaveAttribute("aria-current", "page");
      expect(
        screen.getByRole("button", { name: /SMA Crossover/ }),
      ).not.toHaveAttribute("aria-current");
    });

    it("opens a bot when it is selected", async () => {
      renderLayout();

      await userEvent.click(
        screen.getByRole("button", { name: /Momentum Scalper/ }),
      );

      expect(mockRouter.push).toHaveBeenCalledWith("/dashboard/bot-rt");
    });

    it("says there are no bots yet when the list is empty", () => {
      mockBots = [];
      renderLayout();

      expect(screen.getByText("No bots yet")).toBeInTheDocument();
      expect(screen.getByText("0")).toBeInTheDocument();
    });

    it("filters by search and shows filtered over total", async () => {
      renderLayout();

      await userEvent.type(
        screen.getByRole("textbox", { name: "Filter bots" }),
        "btc",
      );

      expect(screen.getByText("1 / 2")).toBeInTheDocument();
      expect(
        screen.queryByRole("button", { name: /SMA Crossover/ }),
      ).not.toBeInTheDocument();

      await userEvent.type(
        screen.getByRole("textbox", { name: "Filter bots" }),
        "zzz",
      );
      expect(screen.getByText("No matching bots")).toBeInTheDocument();
    });

    it("filters by type and status from the filter menu", async () => {
      renderLayout();

      await userEvent.click(
        screen.getByRole("button", { name: "Filter bots" }),
      );
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
      await userEvent.click(
        screen.getByRole("menuitemradio", { name: "Disabled" }),
      );

      expect(screen.getByText("1 / 2")).toBeInTheDocument();
      expect(
        screen.getByRole("button", { name: "Filter bots (1 active)" }),
      ).toBeInTheDocument();
      expect(
        screen.queryByRole("button", { name: /SMA Crossover/ }),
      ).not.toBeInTheDocument();
    });
  });

  describe("mobile", () => {
    it("renders only the route content without a sidebar", () => {
      setViewport("mobile");
      renderLayout();

      expect(screen.getByTestId("route-content")).toBeInTheDocument();
      expect(screen.queryByText("Bots")).not.toBeInTheDocument();
      expect(screen.queryByRole("separator")).not.toBeInTheDocument();
      expect(screen.getByTestId("route-content").closest("main")).toHaveClass(
        "h-full",
        "overflow-auto",
      );
    });
  });
});
