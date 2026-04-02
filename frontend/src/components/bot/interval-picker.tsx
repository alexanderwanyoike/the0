"use client";

import React, { useState } from "react";
import { cn } from "@/lib/utils";
import { Button } from "@/components/ui/button";
import { Calendar } from "@/components/ui/calendar";
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "@/components/ui/popover";
import { CalendarIcon, Clock, Radio } from "lucide-react";
import type { DateRange } from "react-day-picker";

export interface IntervalValue {
  label: string;
  start: string; // YYYYMMDD format (empty for "live")
  end: string; // YYYYMMDD format (empty for "live")
}

interface IntervalPickerProps {
  value: IntervalValue;
  onChange: (value: IntervalValue) => void;
  showLive?: boolean;
}

function formatDate(date: Date): string {
  const y = date.getFullYear();
  const m = String(date.getMonth() + 1).padStart(2, "0");
  const d = String(date.getDate()).padStart(2, "0");
  return `${y}${m}${d}`;
}

const PRESETS = [
  { label: "1d", days: 1 },
  { label: "3d", days: 3 },
  { label: "7d", days: 7 },
  { label: "30d", days: 30 },
] as const;

export function computeInterval(label: string, days: number): IntervalValue {
  const now = new Date();
  const start = new Date(now);
  start.setDate(start.getDate() - days + 1);
  return {
    label,
    start: formatDate(start),
    end: formatDate(now),
  };
}

export const LIVE_INTERVAL: IntervalValue = {
  label: "live",
  start: "",
  end: "",
};

/** Default interval: last 1 day */
export const DEFAULT_DAY_INTERVAL: IntervalValue = computeInterval("1d", 1);

/** @deprecated Use DEFAULT_DAY_INTERVAL instead */
export const DEFAULT_INTERVAL: IntervalValue = DEFAULT_DAY_INTERVAL;

export function IntervalPicker({
  value,
  onChange,
  showLive = false,
}: IntervalPickerProps) {
  const [calendarOpen, setCalendarOpen] = useState(false);
  const [range, setRange] = useState<DateRange | undefined>(undefined);

  const handleRangeSelect = (selected: DateRange | undefined) => {
    setRange(selected);
    if (selected?.from && selected?.to) {
      onChange({
        label: "custom",
        start: formatDate(selected.from),
        end: formatDate(selected.to),
      });
      setCalendarOpen(false);
      setRange(undefined);
    }
  };

  return (
    <div className="flex flex-wrap items-center gap-1">
      <Clock className="h-3.5 w-3.5 text-muted-foreground mr-1" />

      {showLive && (
        <Button
          variant={value.label === "live" ? "secondary" : "ghost"}
          size="sm"
          className={cn(
            "h-7 px-2.5 text-xs font-mono",
            value.label === "live" &&
              "bg-secondary text-secondary-foreground",
          )}
          onClick={() => onChange(LIVE_INTERVAL)}
        >
          <Radio className="h-3 w-3 mr-1" />
          Live
        </Button>
      )}

      {PRESETS.map((preset) => (
        <Button
          key={preset.label}
          variant={value.label === preset.label ? "secondary" : "ghost"}
          size="sm"
          className={cn(
            "h-7 px-2.5 text-xs font-mono",
            value.label === preset.label &&
              "bg-secondary text-secondary-foreground",
          )}
          onClick={() => onChange(computeInterval(preset.label, preset.days))}
        >
          {preset.label}
        </Button>
      ))}

      <Popover open={calendarOpen} onOpenChange={setCalendarOpen}>
        <PopoverTrigger asChild>
          <Button
            variant={value.label === "custom" ? "secondary" : "ghost"}
            size="sm"
            className={cn(
              "h-7 px-2.5 text-xs font-mono",
              value.label === "custom" &&
                "bg-secondary text-secondary-foreground",
            )}
          >
            <CalendarIcon className="h-3 w-3 mr-1" />
            Custom
          </Button>
        </PopoverTrigger>
        <PopoverContent align="start" className="w-auto p-0">
          <Calendar
            mode="range"
            selected={range}
            onSelect={handleRangeSelect}
            numberOfMonths={2}
            disabled={{ after: new Date() }}
          />
        </PopoverContent>
      </Popover>
    </div>
  );
}
