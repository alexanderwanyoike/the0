"use client";

import { useState } from "react";
import { Bot } from "@/lib/api/api-client";
import { BotListItem } from "./bot-list-item";
import { ScrollArea } from "@/components/ui/scroll-area";
import { Input } from "@/components/ui/input";
import { Badge } from "@/components/ui/badge";
import { Bot as BotIcon, Search } from "lucide-react";
import { cn } from "@/lib/utils";

interface BotListPanelProps {
  bots: Bot[];
  activeBotId: string | null;
  onSelectBot: (botId: string) => void;
  className?: string;
}

export function BotListPanel({
  bots,
  activeBotId,
  onSelectBot,
  className,
}: BotListPanelProps) {
  const [filter, setFilter] = useState("");

  const filtered = bots.filter((bot) => {
    if (!filter) return true;
    const config = bot.config as Record<string, any>;
    const name = (config?.name || bot.id).toLowerCase();
    const symbol = (config?.symbol || "").toLowerCase();
    const q = filter.toLowerCase();
    return name.includes(q) || symbol.includes(q);
  });

  return (
    <div className={cn("flex flex-col", className)}>
      {/* Header */}
      <div className="px-3 py-3 border-b flex-shrink-0">
        <div className="flex items-center justify-between mb-2">
          <span className="text-sm font-medium">Bots</span>
          <Badge variant="secondary" className="text-xs">
            {bots.length}
          </Badge>
        </div>
        <div className="relative">
          <Search className="absolute left-2 top-1/2 -translate-y-1/2 h-3.5 w-3.5 text-muted-foreground" />
          <Input
            placeholder="Filter bots..."
            value={filter}
            onChange={(e) => setFilter(e.target.value)}
            className="h-8 pl-7 text-sm"
          />
        </div>
      </div>

      {/* List */}
      <ScrollArea className="flex-1">
        <div className="p-1.5 space-y-0.5">
          {filtered.length === 0 ? (
            <div className="flex flex-col items-center justify-center py-8 text-muted-foreground">
              <BotIcon className="h-8 w-8 mb-2" />
              <p className="text-sm">
                {bots.length === 0 ? "No bots yet" : "No matching bots"}
              </p>
            </div>
          ) : (
            filtered.map((bot) => (
              <BotListItem
                key={bot.id}
                bot={bot}
                isActive={bot.id === activeBotId}
                onClick={() => onSelectBot(bot.id)}
              />
            ))
          )}
        </div>
      </ScrollArea>
    </div>
  );
}
