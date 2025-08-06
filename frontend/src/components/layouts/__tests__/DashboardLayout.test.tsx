import React from "react";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import DashboardLayout from "../dashboard-layout";
import { useTheme } from "next-themes";

// Mock the auth context
jest.mock("@/contexts/auth-context", () => ({
  useAuth: () => ({
    user: { id: "123", email: "test@example.com", username: "testuser" },
    loading: false,
    logout: jest.fn(),
  }),
}));

// Mock the next/navigation
jest.mock("next/navigation", () => ({
  useRouter: () => ({
    push: jest.fn(),
  }),
  usePathname: () => "/dashboard",
}));

// Mock the next-themes
jest.mock("next-themes", () => ({
  useTheme: () => ({ theme: "light", setTheme: jest.fn() }),
}));

describe("DashboardLayout Component", () => {
  it("renders children and app name", () => {
    render(
      <DashboardLayout>
        <div>Test Content</div>
      </DashboardLayout>,
    );

    // Check that the app logo "0" is rendered (from the sidebar)
    expect(screen.getByText("0")).toBeInTheDocument();
    expect(screen.getByText("Test Content")).toBeInTheDocument();
  });

  it("renders theme toggle button and handles click", async () => {
    const mockSetTheme = jest.fn();
    jest.spyOn(require("next-themes"), "useTheme").mockImplementation(() => ({
      theme: "light",
      setTheme: mockSetTheme,
      themes: ["light", "dark"],
    }));

    render(
      <DashboardLayout>
        <div>Test Content</div>
      </DashboardLayout>,
    );

    const user = userEvent.setup();

    // First click on the theme toggle button to open the dropdown
    const toggleButton = screen.getByRole("button", { name: /toggle theme/i });
    expect(toggleButton).toBeInTheDocument();

    await user.click(toggleButton);

    // Then click on the Dark option
    const darkOption = screen.getByText("Dark");
    await user.click(darkOption);

    expect(mockSetTheme).toHaveBeenCalledWith("dark");
  });
});
