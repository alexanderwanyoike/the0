import { Badge } from "@/components/ui/badge";
import React from "react";
import { STATUS_CONFIG } from "@/components/custom-bots/constants";

export const StatusHeader = ({ bot }: any) => {
  const config = STATUS_CONFIG[bot.status] || STATUS_CONFIG.active;
  const Icon = config.icon;

  return (
    <div className="flex items-start gap-4 p-6 bg-muted/30 rounded-lg border">
      <div className="flex items-center justify-center h-12 w-12 rounded-lg bg-primary/10 shrink-0">
        <Icon className="h-6 w-6 text-primary" />
      </div>
      <div className="flex-1">
        <div className="flex items-center gap-3 mb-2">
          <h2 className="text-xl font-semibold">{config.text}</h2>
          <Badge className={config.color}>{bot.status}</Badge>
        </div>
        <p className="text-muted-foreground">{config.description}</p>
      </div>
    </div>
  );
};
