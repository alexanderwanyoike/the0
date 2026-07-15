import React from "react";
import { render, screen } from "@testing-library/react";
import { ResizableSidebarLayout } from "../resizable-sidebar-layout";

function renderLayout() {
  return render(
    <ResizableSidebarLayout sidebar={<div data-testid="sidebar-content" />}>
      <div data-testid="main-content" />
    </ResizableSidebarLayout>,
  );
}

describe("ResizableSidebarLayout", () => {
  afterEach(() => {
    window.localStorage.clear();
  });

  it("renders the sidebar and main content", () => {
    renderLayout();

    expect(screen.getByTestId("sidebar-content")).toBeInTheDocument();
    expect(screen.getByTestId("main-content")).toBeInTheDocument();
  });

  it("renders a keyboard-accessible resize handle between the panels", () => {
    renderLayout();

    // react-resizable-panels renders the Separator with role="separator"
    // and a tabIndex so it can be resized via arrow keys
    const separator = screen.getByRole("separator");
    expect(separator).toBeInTheDocument();
    expect(separator).toHaveAttribute("tabindex");
  });

  it("restores a saved sidebar layout from localStorage", () => {
    // useDefaultLayout persists under react-resizable-panels:<id>; seeding
    // it simulates a user who resized the sidebar on a previous visit
    window.localStorage.setItem(
      "react-resizable-panels:dashboard-bot-list",
      JSON.stringify({ "bot-list": 30, "dashboard-main": 70 }),
    );

    renderLayout();

    // Panels expose their id as data-testid; flex-grow carries the layout
    const sidebarPanel = screen.getByTestId("bot-list");
    expect(sidebarPanel.style.flexGrow).toBe("30");
  });
});
