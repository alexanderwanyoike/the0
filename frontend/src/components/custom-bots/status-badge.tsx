import React from "react";
import { STATUS_CONFIG } from "@/components/custom-bots/constants";

export const StatusBadge = ({ status }: { status: string }) => {
  const config = STATUS_CONFIG[status] || STATUS_CONFIG.active;
  const Icon = config.icon;

  return (
    <div
      className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium border ${config.color}`}
    >
      <Icon className="w-3 h-3 mr-1" />
      {config.text}
    </div>
  );
};
