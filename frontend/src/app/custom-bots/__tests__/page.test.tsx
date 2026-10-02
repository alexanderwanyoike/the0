import React from "react";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import CustomBotsPage from "../page";
import type {
  CustomBotWithVersions,
  CustomBotVersion,
} from "@/types/custom-bots";

const mockRouter = { push: jest.fn(), replace: jest.fn() };
jest.mock("next/navigation", () => ({
  useRouter: () => mockRouter,
  usePathname: () => "/custom-bots",
}));

let mockBotsState: {
  bots: CustomBotWithVersions[];
  loading: boolean;
  error: string | null;
  refetch: jest.Mock;
};
jest.mock("@/contexts/custom-bots-context", () => ({
  useCustomBotsContext: () => mockBotsState,
}));

function makeBot(
  name: string,
  version: Omit<Partial<CustomBotVersion>, "config"> & {
    config?: Partial<CustomBotVersion["config"]>;
  },
): CustomBotWithVersions {
  return {
    id: `id-${name}`,
    name,
    userId: "user-1",
    latestVersion: version.version ?? "1.0.0",
    createdAt: new Date("2024-01-01"),
    updatedAt: new Date("2024-01-01"),
    versions: [
      {
        id: `v-${name}`,
        version: version.version ?? "1.0.0",
        userId: "user-1",
        createdAt: new Date("2024-01-01"),
        status: version.status ?? "active",
        filePath: `/bots/${name}`,
        config: {
          name,
          version: version.version ?? "1.0.0",
          description: "",
          runtime: "python3.11",
          type: "scheduled",
          author: "test",
          entrypoints: { bot: "main.py" },
          schema: {},
          ...version.config,
        } as CustomBotVersion["config"],
      },
    ],
  };
}

const smaBot = makeBot("sma crossover", {
  version: "1.2.0",
  config: { type: "scheduled", description: "Moving average strategy" },
});
const momentumBot = makeBot("momentum-trader", {
  version: "2.0.0",
  status: "awaiting_human_review" as unknown as CustomBotVersion["status"],
  config: { type: "realtime", description: "" },
});

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
    bots: [smaBot, momentumBot],
    loading: false,
    error: null,
    refetch: jest.fn(),
  };
  setViewport("mobile");
});

describe("CustomBotsPage", () => {
  describe.each(["desktop", "mobile"] as const)(
    "load states on %s",
    (viewport) => {
      beforeEach(() => setViewport(viewport));

      it("shows a spinner while the bots load", () => {
        setBots({ loading: true });
        const { container } = render(<CustomBotsPage />);

        expect(container.querySelector(".animate-spin")).toBeInTheDocument();
        expect(screen.queryByText("Custom Bots")).not.toBeInTheDocument();
        expect(mockRouter.replace).not.toHaveBeenCalled();
      });

      it("shows the error with a retry that refetches the bots", async () => {
        setBots({ error: "Network down" });
        render(<CustomBotsPage />);

        expect(
          screen.getByText("Failed to load custom bots"),
        ).toBeInTheDocument();
        expect(screen.getByText("Network down")).toBeInTheDocument();

        await userEvent.click(
          screen.getByRole("button", { name: /try again/i }),
        );
        expect(mockBotsState.refetch).toHaveBeenCalledTimes(1);
      });

      it("shows the empty state pointing at the CLI", () => {
        setBots({ bots: [] });
        render(<CustomBotsPage />);

        expect(screen.getByText("No Custom Bots")).toBeInTheDocument();
        expect(screen.getByText("the0 custom-bot deploy")).toBeInTheDocument();
        expect(screen.getByRole("link", { name: "View docs" })).toHaveAttribute(
          "href",
          "/docs/the0-CLI/custom-bot-commands",
        );
        expect(mockRouter.replace).not.toHaveBeenCalled();
      });
    },
  );

  describe("on desktop", () => {
    beforeEach(() => setViewport("desktop"));

    it("redirects to the first bot by encoded name and renders nothing itself", () => {
      const { container } = render(<CustomBotsPage />);

      expect(mockRouter.replace).toHaveBeenCalledWith(
        "/custom-bots/sma%20crossover",
      );
      expect(container).toBeEmptyDOMElement();
    });
  });

  describe("mobile list", () => {
    it("shows the title and the bot count", () => {
      render(<CustomBotsPage />);

      expect(
        screen.getByRole("heading", { name: "Custom Bots" }),
      ).toBeInTheDocument();
      expect(screen.getByText("2 bots")).toBeInTheDocument();
    });

    it("uses the singular for a single bot", () => {
      setBots({ bots: [smaBot] });
      render(<CustomBotsPage />);

      expect(screen.getByText("1 bot")).toBeInTheDocument();
    });

    it("renders each bot with its description, type and latest version", () => {
      render(<CustomBotsPage />);

      const sma = screen.getByRole("button", { name: /sma crossover/ });
      expect(sma).toHaveTextContent("Moving average strategy");
      expect(sma).toHaveTextContent("scheduled");
      expect(sma).toHaveTextContent("v1.2.0");
      expect(sma.querySelector(".bg-green-500")).toBeInTheDocument();

      const momentum = screen.getByRole("button", { name: /momentum-trader/ });
      expect(momentum).toHaveTextContent("realtime");
      expect(momentum).toHaveTextContent("v2.0.0");
      expect(momentum.querySelector(".bg-yellow-500")).toBeInTheDocument();
    });

    it("opens a bot by its encoded name and never redirects", async () => {
      render(<CustomBotsPage />);

      await userEvent.click(
        screen.getByRole("button", { name: /sma crossover/ }),
      );

      expect(mockRouter.push).toHaveBeenCalledWith(
        "/custom-bots/sma%20crossover",
      );
      expect(mockRouter.replace).not.toHaveBeenCalled();
    });
  });

  describe("mobile filtering", () => {
    it("narrows the list by name and shows filtered over total", async () => {
      render(<CustomBotsPage />);

      await userEvent.type(
        screen.getByRole("textbox", { name: "Filter bots" }),
        "momentum",
      );

      expect(screen.getByText("1 / 2 bots")).toBeInTheDocument();
      expect(
        screen.queryByRole("button", { name: /sma crossover/ }),
      ).not.toBeInTheDocument();
    });

    it("matches on the description", async () => {
      render(<CustomBotsPage />);

      await userEvent.type(
        screen.getByRole("textbox", { name: "Filter bots" }),
        "average",
      );

      expect(
        screen.getByRole("button", { name: /sma crossover/ }),
      ).toBeInTheDocument();
      expect(screen.getByText("1 / 2 bots")).toBeInTheDocument();
    });

    it("says so when nothing matches", async () => {
      render(<CustomBotsPage />);

      await userEvent.type(
        screen.getByRole("textbox", { name: "Filter bots" }),
        "zzz",
      );

      expect(screen.getByText("No matching bots")).toBeInTheDocument();
      expect(screen.getByText("0 / 2 bots")).toBeInTheDocument();
    });

    it("offers only a type filter", async () => {
      render(<CustomBotsPage />);

      await userEvent.click(
        screen.getByRole("button", { name: "Filter custom bots" }),
      );

      expect(screen.getByText("Type")).toBeInTheDocument();
      expect(screen.queryByText("Status")).not.toBeInTheDocument();
      expect(
        screen.getAllByRole("menuitemradio").map((item) => item.textContent),
      ).toEqual(["All", "Scheduled", "Real-time"]);
    });

    it("filters by type and counts the active filter", async () => {
      render(<CustomBotsPage />);

      await userEvent.click(
        screen.getByRole("button", { name: "Filter custom bots" }),
      );
      await userEvent.click(
        screen.getByRole("menuitemradio", { name: "Real-time" }),
      );

      expect(screen.getByText("1 / 2 bots")).toBeInTheDocument();
      expect(
        screen.getByRole("button", { name: /momentum-trader/ }),
      ).toBeInTheDocument();
      expect(
        screen.getByRole("button", { name: "Filter custom bots (1 active)" }),
      ).toHaveTextContent("1");
    });
  });
});
