import React from "react";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { NavigationMenu } from "../navigation-menu";

jest.mock("@/contexts/auth-context", () => ({
  useAuth: () => ({ user: null, loading: false }),
}));

jest.mock("next-themes", () => ({
  useTheme: () => ({ theme: "light", setTheme: jest.fn() }),
}));

describe("NavigationMenu branding", () => {
  it("shows the app name without a beta label", () => {
    render(<NavigationMenu />);

    expect(screen.getAllByText("the0").length).toBeGreaterThan(0);
    expect(screen.queryByText(/beta/i)).not.toBeInTheDocument();
  });

  it("shows the app name without a beta label in the mobile menu", async () => {
    render(<NavigationMenu />);

    await userEvent
      .setup()
      .click(screen.getByRole("button", { name: /toggle menu/i }));

    expect(screen.getByRole("dialog")).toHaveTextContent("the0");
    expect(screen.queryByText(/beta/i)).not.toBeInTheDocument();
  });
});
