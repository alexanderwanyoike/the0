"use client";

import React from "react";
import { cn } from "@/lib/utils";
import { Button } from "@/components/ui/button";
import { Clock } from "lucide-react";

export interface IntervalValue {
  label: string;
  start: string; // ISO date string
  end: string; // ISO date string
}

interface IntervalPickerProps {
  value: IntervalValue;
  onChange: (value: IntervalValue) => void;
}

const PRESETS = [
  { label: "15m", duration: 15 * 60 * 1000 },
  { label: "1h", duration: 60 * 60 * 1000 },
  { label: "6h", duration: 6 * 60 * 60 * 1000 },
  { label: "1d", duration: 24 * 60 * 60 * 1000 },
] as const;

export function computeInterval(label: string, duration: number): IntervalValue {
  const now = new Date();
  return {
    label,
    start: new Date(now.getTime() - duration).toISOString(),
    end: now.toISOString(),
  };
}

/** Default interval: last 1 day */
export const DEFAULT_INTERVAL: IntervalValue = computeInterval(
  "1d",
  24 * 60 * 60 * 1000,
);

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
          onClick={() => onChange(computeInterval(preset.label, preset.duration))}
        >
          {preset.label}
        </Button>
      ))}
    </div>
  );
}
