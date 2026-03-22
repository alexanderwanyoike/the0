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
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";
import { CustomBotTypeFilter } from "@/hooks/use-custom-bot-filters";

interface CustomBotFilterDropdownProps {
  type: CustomBotTypeFilter;
  setType: (value: CustomBotTypeFilter) => void;
  activeCount: number;
}

export function CustomBotFilterDropdown({
  type,
  setType,
  activeCount,
}: CustomBotFilterDropdownProps) {
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button
          variant="outline"
          size="icon"
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
          onValueChange={(v) => setType(v as CustomBotTypeFilter)}
        >
          <DropdownMenuRadioItem value="all">All</DropdownMenuRadioItem>
          <DropdownMenuRadioItem value="scheduled">
            Scheduled
          </DropdownMenuRadioItem>
          <DropdownMenuRadioItem value="realtime">
            Real-time
          </DropdownMenuRadioItem>
        </DropdownMenuRadioGroup>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
