"use client";

import { CustomBotWithVersions } from "@/types/custom-bots";
import { cn } from "@/lib/utils";
import { Badge } from "@/components/ui/badge";
import { MobileBotCard } from "@/components/bot-list/bot-list";

interface CustomBotListItemProps {
  bot: CustomBotWithVersions;
  isActive: boolean;
  onClick: () => void;
}

export function CustomBotListItem({
  bot,
  isActive,
  onClick,
}: CustomBotListItemProps) {
  const latestVersionData = bot.versions[0];
  const config = latestVersionData?.config;
  const type = config?.type || "";
  const status = latestVersionData?.status;
  const statusColor = status === "active" ? "bg-green-500" : "bg-yellow-500";

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
          className={cn("h-2 w-2 rounded-full flex-shrink-0", statusColor)}
        />
        <div className="min-w-0 flex-1">
          <p className="text-sm font-medium truncate">{bot.name}</p>
          <div className="flex items-center gap-1.5 mt-0.5">
            {type && (
              <Badge variant="outline" className="text-[10px] px-1 py-0">
                {type}
              </Badge>
            )}
            <span className="text-[10px] text-muted-foreground">
              v{bot.latestVersion}
            </span>
          </div>
        </div>
      </div>
    </button>
  );
}

export function MobileCustomBotListItem({
  bot,
  onClick,
}: {
  bot: CustomBotWithVersions;
  onClick: () => void;
}) {
  const config = bot.versions[0]?.config;
  const botType = config?.type || "Bot";
  const description = config?.description || "";
  const status = bot.versions[0]?.status;

  return (
    <MobileBotCard
      statusColor={status === "active" ? "bg-green-500" : "bg-yellow-500"}
      onClick={onClick}
    >
      <div className="min-w-0 flex-1">
        <p className="text-sm font-medium truncate">{bot.name}</p>
        {description && (
          <p className="text-xs text-muted-foreground truncate mt-0.5">
            {description}
          </p>
        )}
        <div className="flex items-center gap-2 mt-1">
          <Badge variant="outline" className="text-xs">
            {botType}
          </Badge>
          <span className="text-xs text-muted-foreground">
            v{bot.latestVersion}
          </span>
        </div>
      </div>
    </MobileBotCard>
  );
}
