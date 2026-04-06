import React from "react";
import { render, screen, fireEvent } from "@testing-library/react";
import { RefreshSelector, shouldHideRefreshSelector } from "../refresh-selector";

describe("RefreshSelector", () => {
  const mockOnChange = jest.fn();

  beforeEach(() => {
    mockOnChange.mockClear();
  });

  it("should render refresh interval options", () => {
    render(<RefreshSelector value={30000} onChange={mockOnChange} />);

    expect(screen.getByRole("button", { name: /10s/i })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: /30s/i })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: /60s/i })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: /off/i })).toBeInTheDocument();
  });

  it("should call onChange when option selected", () => {
    render(<RefreshSelector value={30000} onChange={mockOnChange} />);

    fireEvent.click(screen.getByRole("button", { name: /10s/i }));

    expect(mockOnChange).toHaveBeenCalledTimes(1);
    expect(mockOnChange).toHaveBeenCalledWith(10000);
  });

  it("should highlight active option", () => {
    render(<RefreshSelector value={30000} onChange={mockOnChange} />);

    const activeButton = screen.getByRole("button", { name: /30s/i });
    const inactiveButton = screen.getByRole("button", { name: /10s/i });

    expect(activeButton.className).toContain("bg-secondary");
    expect(inactiveButton.className).not.toContain("bg-secondary");
  });

  it("should highlight Off when value is 0", () => {
    render(<RefreshSelector value={0} onChange={mockOnChange} />);

    const offButton = screen.getByRole("button", { name: /off/i });
    expect(offButton.className).toContain("bg-secondary");
  });

  it("should call onChange with 0 when Off is clicked", () => {
    render(<RefreshSelector value={30000} onChange={mockOnChange} />);

    fireEvent.click(screen.getByRole("button", { name: /off/i }));

    expect(mockOnChange).toHaveBeenCalledWith(0);
  });

  it("should render nothing when hidden is true", () => {
    const { container } = render(
      <RefreshSelector value={30000} onChange={mockOnChange} hidden />,
    );
    expect(container.innerHTML).toBe("");
  });

  it("should render when hidden is false", () => {
    render(
      <RefreshSelector value={30000} onChange={mockOnChange} hidden={false} />,
    );
    expect(screen.getByRole("button", { name: /30s/i })).toBeInTheDocument();
  });

  describe("shouldHideRefreshSelector", () => {
    it("should be visible for scheduled bots regardless of interval", () => {
      expect(shouldHideRefreshSelector(false, "1d")).toBe(false);
      expect(shouldHideRefreshSelector(false, "1h")).toBe(false);
      expect(shouldHideRefreshSelector(false, "live")).toBe(false);
      expect(shouldHideRefreshSelector(false, "custom")).toBe(false);
    });

    it("should be hidden for realtime bots in live mode", () => {
      expect(shouldHideRefreshSelector(true, "live")).toBe(true);
    });

    it("should be visible for realtime bots with rolling hour presets", () => {
      expect(shouldHideRefreshSelector(true, "1h")).toBe(false);
      expect(shouldHideRefreshSelector(true, "6h")).toBe(false);
    });

    it("should be hidden for realtime bots with day presets", () => {
      expect(shouldHideRefreshSelector(true, "1d")).toBe(true);
      expect(shouldHideRefreshSelector(true, "3d")).toBe(true);
      expect(shouldHideRefreshSelector(true, "7d")).toBe(true);
      expect(shouldHideRefreshSelector(true, "30d")).toBe(true);
    });

    it("should be hidden for realtime bots with custom range", () => {
      expect(shouldHideRefreshSelector(true, "custom")).toBe(true);
    });
  });
});
