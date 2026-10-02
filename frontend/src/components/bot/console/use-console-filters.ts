"use client";

import { useCallback, useState } from "react";

const compactDate = (value: string) => value.replace(/-/g, "");

/**
 * Client-side search plus the single-date / date-range pickers. A single
 * date and a range are mutually exclusive: applying one clears the other.
 * Dates leave as compact YYYYMMDD strings.
 */
export function useConsoleFilters(
  onDateChange: (date: string | null) => void,
  onDateRangeChange: (startDate: string, endDate: string) => void,
) {
  const [searchQuery, setSearchQuery] = useState("");
  const [selectedDate, setSelectedDate] = useState<string>("");
  const [dateRange, setDateRange] = useState<{ start: string; end: string }>({
    start: "",
    end: "",
  });

  const handleDateChange = useCallback(
    (value: string) => {
      setSelectedDate(value);
      if (value) {
        onDateChange(compactDate(value));
        setDateRange({ start: "", end: "" });
      } else {
        onDateChange(null);
      }
    },
    [onDateChange],
  );

  const applyRange = useCallback(
    (start: string, end: string) => {
      if (start && end) {
        onDateRangeChange(compactDate(start), compactDate(end));
        setSelectedDate("");
      }
    },
    [onDateRangeChange],
  );

  const handleRangeStartChange = (start: string) => {
    setDateRange((prev) => ({ ...prev, start }));
    if (start && dateRange.end) applyRange(start, dateRange.end);
  };

  const handleRangeEndChange = (end: string) => {
    setDateRange((prev) => ({ ...prev, end }));
    if (end && dateRange.start) applyRange(dateRange.start, end);
  };

  const clearFilters = useCallback(() => {
    setSearchQuery("");
    setSelectedDate("");
    setDateRange({ start: "", end: "" });
    onDateChange(null);
  }, [onDateChange]);

  return {
    searchQuery,
    setSearchQuery,
    selectedDate,
    dateRange,
    hasActiveFilters: !!(searchQuery || selectedDate || dateRange.start),
    handleDateChange,
    handleRangeStartChange,
    handleRangeEndChange,
    clearFilters,
  };
}

export type ConsoleFilters = ReturnType<typeof useConsoleFilters>;
