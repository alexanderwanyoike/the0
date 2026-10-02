import React from "react";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import Layout from "../layout";
import type {
  CustomBotWithVersions,
  CustomBotVersion,
} from "@/types/custom-bots";

const mockRouter = { push: jest.fn(), replace: jest.fn() };
let mockPathname = "/custom-bots";
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

let mockBots: CustomBotWithVersions[];
jest.mock("@/contexts/custom-bots-context", () => ({
  CustomBotsProvider: ({ children }: { children: React.ReactNode }) => (
    <div data-testid="custom-bots-provider">{children}</div>
  ),
  useCustomBotsContext: () => ({
    bots: mockBots,
    loading: false,
    error: null,
    refetch: jest.fn(),
  }),
}));

function makeBot(
  name: string,
  version: string,
  type: string,
): CustomBotWithVersions {
  return {
    id: `id-${name}`,
    name,
    userId: "user-1",
    latestVersion: version,
    createdAt: new Date("2024-01-01"),
    updatedAt: new Date("2024-01-01"),
    versions: [
      {
        id: `v-${name}`,
        version,
        userId: "user-1",
        createdAt: new Date("2024-01-01"),
        status: "active",
        filePath: `/bots/${name}`,
        config: {
          name,
          version,
          description: "",
          runtime: "python3.11",
          type,
          author: "test",
          entrypoints: { bot: "main.py" },
          schema: {},
        } as CustomBotVersion["config"],
      },
    ],
  };
}

const smaBot = makeBot("sma crossover", "1.2.0", "scheduled");
const momentumBot = makeBot("momentum-trader", "2.0.0", "realtime");

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
  mockPathname = "/custom-bots";
  mockAuth = { user: { id: "user-1" }, loading: false };
  mockBots = [smaBot, momentumBot];
  setViewport("desktop");
});

describe("custom bots layout", () => {
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

    it("renders the route inside the custom bots provider once signed in", () => {
      renderLayout();

      const provider = screen.getByTestId("custom-bots-provider");
      expect(screen.getByTestId("app-chrome")).toContainElement(provider);
      expect(provider).toContainElement(screen.getByTestId("route-content"));
    });
  });

  describe("desktop sidebar", () => {
    it("lists the bots under a header with the total", () => {
      renderLayout();

      expect(screen.getByText("Custom Bots")).toBeInTheDocument();
      expect(screen.getByText("2")).toBeInTheDocument();
      const sma = screen.getByRole("button", { name: /sma crossover/ });
      expect(sma).toHaveTextContent("scheduled");
      expect(sma).toHaveTextContent("v1.2.0");
    });

    it("sits in a fixed-width aside next to the route content", () => {
      renderLayout();

      const aside = screen.getByRole("complementary");
      expect(aside).toHaveClass("w-[220px]", "border-r", "flex-shrink-0");
      expect(aside).toContainElement(screen.getByText("Custom Bots"));
      expect(screen.queryByRole("separator")).not.toBeInTheDocument();
      expect(screen.getByTestId("route-content").closest("main")).toHaveClass(
        "flex-1",
        "overflow-auto",
      );
    });

    it("marks the bot named in the URL as the current one", () => {
      mockPathname = "/custom-bots/sma%20crossover";
      renderLayout();

      expect(
        screen.getByRole("button", { name: /sma crossover/ }),
      ).toHaveAttribute("aria-current", "page");
      expect(
        screen.getByRole("button", { name: /momentum-trader/ }),
      ).not.toHaveAttribute("aria-current");
    });

    it("opens a bot by its encoded name when it is selected", async () => {
      renderLayout();

      await userEvent.click(
        screen.getByRole("button", { name: /sma crossover/ }),
      );

      expect(mockRouter.push).toHaveBeenCalledWith(
        "/custom-bots/sma%20crossover",
      );
    });

    it("says there are no custom bots yet when the list is empty", () => {
      mockBots = [];
      renderLayout();

      expect(screen.getByText("No custom bots yet")).toBeInTheDocument();
      expect(screen.getByText("0")).toBeInTheDocument();
    });

    it("filters by search and shows filtered over total", async () => {
      renderLayout();

      await userEvent.type(
        screen.getByRole("textbox", { name: "Filter bots" }),
        "momentum",
      );

      expect(screen.getByText("1 / 2")).toBeInTheDocument();
      expect(
        screen.queryByRole("button", { name: /sma crossover/ }),
      ).not.toBeInTheDocument();

      await userEvent.type(
        screen.getByRole("textbox", { name: "Filter bots" }),
        "zzz",
      );
      expect(screen.getByText("No matching bots")).toBeInTheDocument();
    });

    it("filters by type from the filter menu", async () => {
      renderLayout();

      await userEvent.click(
        screen.getByRole("button", { name: "Filter custom bots" }),
      );
      expect(
        screen.getAllByRole("menuitemradio").map((item) => item.textContent),
      ).toEqual(["All", "Scheduled", "Real-time"]);
      await userEvent.click(
        screen.getByRole("menuitemradio", { name: "Scheduled" }),
      );

      expect(screen.getByText("1 / 2")).toBeInTheDocument();
      expect(
        screen.getByRole("button", { name: "Filter custom bots (1 active)" }),
      ).toBeInTheDocument();
      expect(
        screen.queryByRole("button", { name: /momentum-trader/ }),
      ).not.toBeInTheDocument();
    });
  });

  describe("mobile", () => {
    it("renders only the route content without a sidebar", () => {
      setViewport("mobile");
      renderLayout();

      expect(screen.getByTestId("route-content")).toBeInTheDocument();
      expect(screen.queryByRole("complementary")).not.toBeInTheDocument();
      expect(screen.getByTestId("route-content").closest("main")).toHaveClass(
        "h-full",
        "overflow-auto",
      );
    });
  });
});
