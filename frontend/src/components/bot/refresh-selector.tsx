"use client";

import React from "react";
import { cn } from "@/lib/utils";
import { Button } from "@/components/ui/button";
import { RefreshCw } from "lucide-react";

export interface RefreshSelectorProps {
  value: number; // ms, 0 = off
  onChange: (ms: number) => void;
}

const OPTIONS = [
  { label: "10s", ms: 10000 },
  { label: "30s", ms: 30000 },
  { label: "60s", ms: 60000 },
  { label: "Off", ms: 0 },
] as const;

export function RefreshSelector({ value, onChange }: RefreshSelectorProps) {
  return (
    <div className="flex items-center gap-1">
      <RefreshCw className="h-3.5 w-3.5 text-muted-foreground mr-1" />

      {OPTIONS.map((option) => (
        <Button
          key={option.label}
          variant={value === option.ms ? "secondary" : "ghost"}
          size="sm"
          className={cn(
            "h-7 px-2.5 text-xs font-mono",
            value === option.ms &&
              "bg-secondary text-secondary-foreground",
          )}
          onClick={() => onChange(option.ms)}
        >
          {option.label}
        </Button>
      ))}
    </div>
  );
}
