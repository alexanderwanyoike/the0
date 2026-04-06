import React from "react";
import { render, screen, fireEvent } from "@testing-library/react";
import { RefreshSelector } from "../refresh-selector";

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
});
