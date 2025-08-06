// __tests__/Sidebar.test.tsx
import React from "react";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { Sidebar } from "../sidebar";
import { AuthUser } from "@/lib/auth/types";

const mockNavigation = [
  { name: "Dashboard", href: "/dashboard", icon: () => <div>Icon</div> },
  { name: "Analytics", href: "/analytics", icon: () => <div>Icon</div> },
];

const mockUser: AuthUser = {
  id: "123",
  email: "test@example.com",
  username: "Test User",
  firstName: "Test",
  lastName: "User",
  isActive: true,
  isEmailVerified: true,
};

describe("Sidebar Component", () => {
  it("renders app name and navigation items", () => {
    render(
      <Sidebar
        navigation={mockNavigation}
        user={mockUser}
        onLogout={jest.fn()}
        currentPath="/dashboard"
        isCollapsed={false}
        onCollapsedChange={jest.fn()}
      />,
    );

    expect(screen.getByText("the0")).toBeInTheDocument();
    expect(screen.getByText("Dashboard")).toBeInTheDocument();
    expect(screen.getByText("Analytics")).toBeInTheDocument();
  });

  it("calls onLogout when logout button is clicked", async () => {
    const mockOnLogout = jest.fn();
    render(
      <Sidebar
        navigation={mockNavigation}
        user={mockUser}
        onLogout={mockOnLogout}
        currentPath="/dashboard"
        isCollapsed={false}
        onCollapsedChange={jest.fn()}
      />,
    );

    const user = userEvent.setup();

    // First click on the user avatar to open the dropdown
    const userButton = screen.getByRole("button", { name: /test user/i });
    await user.click(userButton);

    // Then click on the logout option
    const logoutButton = screen.getByText("Log out");
    await user.click(logoutButton);

    expect(mockOnLogout).toHaveBeenCalled();
  });

  it("displays user name when user is provided", () => {
    render(
      <Sidebar
        navigation={mockNavigation}
        user={mockUser}
        onLogout={jest.fn()}
        currentPath="/dashboard"
        isCollapsed={false}
        onCollapsedChange={jest.fn()}
      />,
    );

    expect(screen.getByText("Test User")).toBeInTheDocument();
  });

  it("calls onCollapsedChange when collapse toggle is clicked", async () => {
    const mockOnCollapsedChange = jest.fn();
    render(
      <Sidebar
        navigation={mockNavigation}
        user={mockUser}
        onLogout={jest.fn()}
        currentPath="/dashboard"
        isCollapsed={false}
        onCollapsedChange={mockOnCollapsedChange}
      />,
    );

    const user = userEvent.setup();
    const collapseButtons = screen.getAllByRole("button");
    // Find the first button which should be the collapse toggle (not the dropdown trigger)
    const collapseButton = collapseButtons[0];

    await user.click(collapseButton);

    expect(mockOnCollapsedChange).toHaveBeenCalledWith(true);
  });
});
