import React from "react";
import { render, screen, fireEvent } from "@testing-library/react";
import {
  IntervalPicker,
  LIVE_INTERVAL,
  DEFAULT_DAY_INTERVAL,
  computeInterval,
} from "../interval-picker";

// Mock the Popover/Calendar to avoid portal rendering issues in tests
jest.mock("@/components/ui/popover", () => ({
  Popover: ({ children }: any) => <div data-testid="popover">{children}</div>,
  PopoverTrigger: ({ children, asChild }: any) => (
    <div data-testid="popover-trigger">{asChild ? children : children}</div>
  ),
  PopoverContent: ({ children }: any) => (
    <div data-testid="popover-content">{children}</div>
  ),
}));

jest.mock("@/components/ui/calendar", () => ({
  Calendar: (props: any) => (
    <div data-testid="calendar" data-mode={props.mode} />
  ),
}));

describe("IntervalPicker", () => {
  const mockOnChange = jest.fn();

  beforeEach(() => {
    mockOnChange.mockClear();
  });

  it("should render all presets including hour presets", () => {
    render(
      <IntervalPicker
        value={DEFAULT_DAY_INTERVAL}
        onChange={mockOnChange}
      />
    );

    expect(screen.getByRole("button", { name: "1h" })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "6h" })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "1d" })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "3d" })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "7d" })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "30d" })).toBeInTheDocument();
  });

  it("should render Live button when showLive is true", () => {
    render(
      <IntervalPicker
        value={DEFAULT_DAY_INTERVAL}
        onChange={mockOnChange}
        showLive
      />
    );

    expect(screen.getByRole("button", { name: /live/i })).toBeInTheDocument();
  });

  it("should not render Live button when showLive is false", () => {
    render(
      <IntervalPicker
        value={DEFAULT_DAY_INTERVAL}
        onChange={mockOnChange}
        showLive={false}
      />
    );

    expect(screen.queryByRole("button", { name: /live/i })).not.toBeInTheDocument();
  });

  it("should not render Live button when showLive is omitted", () => {
    render(
      <IntervalPicker
        value={DEFAULT_DAY_INTERVAL}
        onChange={mockOnChange}
      />
    );

    expect(screen.queryByRole("button", { name: /live/i })).not.toBeInTheDocument();
  });

  it("should call onChange with live interval when Live clicked", () => {
    render(
      <IntervalPicker
        value={DEFAULT_DAY_INTERVAL}
        onChange={mockOnChange}
        showLive
      />
    );

    fireEvent.click(screen.getByRole("button", { name: /live/i }));

    expect(mockOnChange).toHaveBeenCalledTimes(1);
    expect(mockOnChange).toHaveBeenCalledWith(LIVE_INTERVAL);
  });

  it("should call onChange with computed date range when day preset clicked", () => {
    render(
      <IntervalPicker
        value={LIVE_INTERVAL}
        onChange={mockOnChange}
        showLive
      />
    );

    fireEvent.click(screen.getByRole("button", { name: "7d" }));

    expect(mockOnChange).toHaveBeenCalledTimes(1);
    const result = mockOnChange.mock.calls[0][0];
    expect(result.label).toBe("7d");
    // start and end should be YYYYMMDD format (8 digits)
    expect(result.start).toMatch(/^\d{8}$/);
    expect(result.end).toMatch(/^\d{8}$/);
  });

  it("should call onChange with ISO datetime range when hour preset clicked", () => {
    render(
      <IntervalPicker
        value={LIVE_INTERVAL}
        onChange={mockOnChange}
        showLive
      />
    );

    fireEvent.click(screen.getByRole("button", { name: "1h" }));

    expect(mockOnChange).toHaveBeenCalledTimes(1);
    const result = mockOnChange.mock.calls[0][0];
    expect(result.label).toBe("1h");
    // Hour presets now return ISO datetime strings
    expect(result.start).toContain("T");
    expect(result.end).toContain("T");
    // start should be before end
    expect(new Date(result.start).getTime()).toBeLessThan(
      new Date(result.end).getTime(),
    );
  });

  it("should highlight the active interval", () => {
    const interval7d = computeInterval("7d", 7);
    render(
      <IntervalPicker
        value={interval7d}
        onChange={mockOnChange}
      />
    );

    const activeButton = screen.getByRole("button", { name: "7d" });
    const inactiveButton = screen.getByRole("button", { name: "1d" });

    // Active button should use "secondary" variant styling
    expect(activeButton.className).toContain("bg-secondary");
    // Inactive button should NOT have secondary styling
    expect(inactiveButton.className).not.toContain("bg-secondary");
  });

  it("should highlight Live when active", () => {
    render(
      <IntervalPicker
        value={LIVE_INTERVAL}
        onChange={mockOnChange}
        showLive
      />
    );

    const liveButton = screen.getByRole("button", { name: /live/i });
    expect(liveButton.className).toContain("bg-secondary");
  });

  it("should render Custom button", () => {
    render(
      <IntervalPicker
        value={DEFAULT_DAY_INTERVAL}
        onChange={mockOnChange}
      />
    );

    expect(screen.getByRole("button", { name: /custom/i })).toBeInTheDocument();
  });

  describe("LIVE_INTERVAL constant", () => {
    it("should have label 'live' and empty start/end", () => {
      expect(LIVE_INTERVAL).toEqual({
        type: "live",
        label: "live",
        start: "",
        end: "",
      });
    });
  });

  describe("DEFAULT_DAY_INTERVAL constant", () => {
    it("should have label '1d' and YYYYMMDD start/end", () => {
      expect(DEFAULT_DAY_INTERVAL.label).toBe("1d");
      expect(DEFAULT_DAY_INTERVAL.start).toMatch(/^\d{8}$/);
      expect(DEFAULT_DAY_INTERVAL.end).toMatch(/^\d{8}$/);
    });
  });

  describe("computeInterval", () => {
    it("should compute correct date range for given days", () => {
      const result = computeInterval("3d", { days: 3 });
      expect(result.label).toBe("3d");
      expect(result.start).toMatch(/^\d{8}$/);
      expect(result.end).toMatch(/^\d{8}$/);
      // end should be today
      const today = new Date();
      const expectedEnd =
        String(today.getUTCFullYear()) +
        String(today.getUTCMonth() + 1).padStart(2, "0") +
        String(today.getUTCDate()).padStart(2, "0");
      expect(result.end).toBe(expectedEnd);
    });

    it("should return ISO datetime range for hour presets", () => {
      const result1h = computeInterval("1h", { hours: 1 });
      expect(result1h.label).toBe("1h");
      expect(result1h.start).toContain("T");
      expect(result1h.end).toContain("T");

      // The start should be approximately 1 hour before end
      const startMs = new Date(result1h.start).getTime();
      const endMs = new Date(result1h.end).getTime();
      const diffHours = (endMs - startMs) / (60 * 60 * 1000);
      expect(diffHours).toBeCloseTo(1, 0);

      const result6h = computeInterval("6h", { hours: 6 });
      expect(result6h.label).toBe("6h");
      expect(result6h.start).toContain("T");
      expect(result6h.end).toContain("T");

      const diff6h =
        (new Date(result6h.end).getTime() -
          new Date(result6h.start).getTime()) /
        (60 * 60 * 1000);
      expect(diff6h).toBeCloseTo(6, 0);
    });
  });
});
