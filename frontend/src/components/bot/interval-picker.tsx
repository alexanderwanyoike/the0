"use client";

import React from "react";
import { cn } from "@/lib/utils";
import { Button } from "@/components/ui/button";
import { Clock } from "lucide-react";

export interface IntervalValue {
  label: string;
  start: string; // YYYYMMDD format
  end: string; // YYYYMMDD format
}

interface IntervalPickerProps {
  value: IntervalValue;
  onChange: (value: IntervalValue) => void;
}

function formatDate(date: Date): string {
  return date.toISOString().slice(0, 10).replace(/-/g, "");
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

/** Default interval: last 1 day */
export const DEFAULT_INTERVAL: IntervalValue = computeInterval("1d", 1);

export function IntervalPicker({ value, onChange }: IntervalPickerProps) {
  return (
    <div className="flex items-center gap-1">
      <Clock className="h-3.5 w-3.5 text-muted-foreground mr-1" />
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
    </div>
  );
}
