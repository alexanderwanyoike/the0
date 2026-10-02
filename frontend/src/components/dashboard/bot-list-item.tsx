"use client";

import cronstrue from "cronstrue";
import { Clock } from "lucide-react";
import { Bot } from "@/lib/api/api-client";
import { cn } from "@/lib/utils";
import { Badge } from "@/components/ui/badge";
import { MobileBotCard } from "@/components/bot-list/bot-list";

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
      aria-current={isActive ? "page" : undefined}
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
          <p className="text-sm font-medium truncate" title={name}>
            {name}
          </p>
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

function readableSchedule(schedule: string | undefined) {
  if (!schedule) return "Real-time";
  try {
    return cronstrue.toString(schedule);
  } catch {
    return schedule;
  }
}

export function MobileBotListItem({
  bot,
  onClick,
}: {
  bot: Bot;
  onClick: () => void;
}) {
  const config = bot.config as Record<string, any>;
  const name = config?.name || bot.id;
  const symbol = config?.symbol || "";
  const botType = config?.type || "Bot";
  const enabled = config?.enabled ?? true;

  return (
    <MobileBotCard
      statusColor={enabled ? "bg-green-500" : "bg-gray-400"}
      onClick={onClick}
    >
      <div className="min-w-0 flex-1">
        <p className="text-sm font-medium truncate">{name}</p>
        <div className="flex items-center gap-2 mt-1">
          {symbol && (
            <Badge variant="secondary" className="text-xs font-mono">
              {symbol}
            </Badge>
          )}
          <Badge variant="outline" className="text-xs">
            {botType}
          </Badge>
        </div>
      </div>
      <div className="flex items-center gap-1 text-xs text-muted-foreground flex-shrink-0">
        <Clock className="h-3 w-3" />
        <span className="hidden sm:inline">
          {readableSchedule(config?.schedule)}
        </span>
      </div>
    </MobileBotCard>
  );
}
