"use client";

import { Bot } from "@/lib/api/api-client";
import { cn } from "@/lib/utils";

interface BotListItemProps {
  bot: Bot;
  isActive: boolean;
  onClick: () => void;
}

export function BotListItem({ bot, isActive, onClick }: BotListItemProps) {
  const config = bot.config as Record<string, any>;
  const name = config?.name || bot.id;
  const symbol = config?.symbol || "";
  const enabled = config?.enabled ?? true;

  return (
    <button
      onClick={onClick}
      className={cn(
        "w-full text-left px-3 py-2.5 rounded-md transition-colors",
        "hover:bg-accent/50 cursor-pointer",
        "border-l-2 border-transparent",
        isActive && "bg-accent border-l-primary",
      )}
    >
      <div className="flex items-center gap-2 min-w-0">
        <span
          className={cn(
            "h-2 w-2 rounded-full flex-shrink-0",
            enabled ? "bg-green-500" : "bg-gray-400",
          )}
        />
        <div className="min-w-0 flex-1">
          <p className="text-sm font-medium truncate">{name}</p>
          {symbol && (
            <p className="text-xs text-muted-foreground font-mono truncate">
              {symbol}
            </p>
          )}
        </div>
      </div>
    </button>
  );
}
