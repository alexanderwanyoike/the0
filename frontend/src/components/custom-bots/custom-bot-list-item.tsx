"use client";

import { CustomBotWithVersions } from "@/types/custom-bots";
import { cn } from "@/lib/utils";
import { Badge } from "@/components/ui/badge";

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
        <span className="h-2 w-2 rounded-full flex-shrink-0 bg-green-500" />
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
