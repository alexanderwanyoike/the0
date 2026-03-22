"use client";

import { Filter } from "lucide-react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuLabel,
  DropdownMenuRadioGroup,
  DropdownMenuRadioItem,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";
import { BotTypeFilter, BotStatusFilter } from "@/hooks/use-bot-filters";

interface BotFilterDropdownProps {
  type: BotTypeFilter;
  setType: (value: BotTypeFilter) => void;
  status: BotStatusFilter;
  setStatus: (value: BotStatusFilter) => void;
  activeCount: number;
}

export function BotFilterDropdown({
  type,
  setType,
  status,
  setStatus,
  activeCount,
}: BotFilterDropdownProps) {
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button
          variant="outline"
          size="icon"
          aria-label={`Filter bots${activeCount > 0 ? ` (${activeCount} active)` : ""}`}
          className="h-8 w-8 flex-shrink-0 relative"
        >
          <Filter className="h-3.5 w-3.5" />
          {activeCount > 0 && (
            <Badge
              variant="secondary"
              className="absolute -top-1.5 -right-1.5 h-4 w-4 p-0 flex items-center justify-center text-[10px]"
            >
              {activeCount}
            </Badge>
          )}
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent align="start" className="w-44">
        <DropdownMenuLabel className="text-xs">Type</DropdownMenuLabel>
        <DropdownMenuRadioGroup
          value={type}
          onValueChange={(v) => setType(v as BotTypeFilter)}
        >
          <DropdownMenuRadioItem value="all">All</DropdownMenuRadioItem>
          <DropdownMenuRadioItem value="scheduled">
            Scheduled
          </DropdownMenuRadioItem>
          <DropdownMenuRadioItem value="realtime">
            Real-time
          </DropdownMenuRadioItem>
        </DropdownMenuRadioGroup>
        <DropdownMenuSeparator />
        <DropdownMenuLabel className="text-xs">Status</DropdownMenuLabel>
        <DropdownMenuRadioGroup
          value={status}
          onValueChange={(v) => setStatus(v as BotStatusFilter)}
        >
          <DropdownMenuRadioItem value="all">All</DropdownMenuRadioItem>
          <DropdownMenuRadioItem value="enabled">Enabled</DropdownMenuRadioItem>
          <DropdownMenuRadioItem value="disabled">
            Disabled
          </DropdownMenuRadioItem>
        </DropdownMenuRadioGroup>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
