"use client";

import React from "react";
import moment from "moment";
import {
  AlertTriangle,
  Clipboard,
  Loader2,
  Terminal,
  Trash2,
} from "lucide-react";
import { Button } from "@/components/ui/button";
import { Switch } from "@/components/ui/switch";
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
  AlertDialogTrigger,
} from "@/components/ui/alert-dialog";
import type { Bot } from "@/lib/api/api-client";

const DeleteBotDialog = ({
  isDeleting,
  onDelete,
}: {
  isDeleting: boolean;
  onDelete: () => void;
}) => (
  <AlertDialog>
    <AlertDialogTrigger asChild>
      <Button
        variant="outline"
        size="sm"
        className="text-destructive border-destructive hover:bg-destructive/10"
      >
        <Trash2 className="h-4 w-4 mr-2" />
        Delete
      </Button>
    </AlertDialogTrigger>
    <AlertDialogContent>
      <AlertDialogHeader>
        <AlertDialogTitle>Delete Bot</AlertDialogTitle>
        <AlertDialogDescription>
          Are you sure you want to delete this bot? This action cannot be undone
          and all trading activity will immediately cease.
        </AlertDialogDescription>
      </AlertDialogHeader>
      <AlertDialogFooter>
        <AlertDialogCancel>Cancel</AlertDialogCancel>
        <AlertDialogAction
          onClick={onDelete}
          className="bg-destructive text-destructive-foreground hover:bg-destructive/90"
          disabled={isDeleting}
        >
          {isDeleting ? (
            <>
              <Loader2 className="h-4 w-4 mr-2 animate-spin" />
              Deleting...
            </>
          ) : (
            "Delete Bot"
          )}
        </AlertDialogAction>
      </AlertDialogFooter>
    </AlertDialogContent>
  </AlertDialog>
);

interface BotDetailHeaderProps {
  bot: Bot;
  isUpdatingEnabled: boolean;
  isDeleting: boolean;
  onToggleEnabled: (enabled: boolean) => void;
  onOpenUpdateModal: () => void;
  onDelete: () => void;
}

export const BotDetailHeader = ({
  bot,
  isUpdatingEnabled,
  isDeleting,
  onToggleEnabled,
  onOpenUpdateModal,
  onDelete,
}: BotDetailHeaderProps) => {
  const enabled = bot.config.enabled ?? true;

  return (
    <div className="border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60">
      <div className="p-4 lg:px-6 lg:py-4">
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-4">
            <h1 className="text-lg font-medium">{bot.config.name}</h1>
          </div>
          <div className="flex items-center gap-2">
            <p className="text-sm text-muted-foreground font-mono">
              {bot.id.slice(-6)}
            </p>
            <div className="flex items-center gap-2">
              <Switch
                checked={enabled}
                onCheckedChange={onToggleEnabled}
                disabled={isUpdatingEnabled}
              />
              <span className="text-sm text-muted-foreground">
                {isUpdatingEnabled ? (
                  <Loader2 className="h-4 w-4 animate-spin" />
                ) : enabled ? (
                  "Enabled"
                ) : (
                  "Disabled"
                )}
              </span>
              <Button variant="outline" size="sm" onClick={onOpenUpdateModal}>
                <Terminal className="h-4 w-4 mr-2" />
                Update via CLI
              </Button>
              <DeleteBotDialog isDeleting={isDeleting} onDelete={onDelete} />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

const DetailRow = ({
  label,
  children,
}: {
  label: string;
  children: React.ReactNode;
}) => (
  <div className="grid grid-cols-3 gap-1">
    <dt className="text-sm text-muted-foreground">{label}</dt>
    {children}
  </div>
);

const DateTimeCell = ({ value }: { value: string | Date }) => (
  <dd className="col-span-2">
    <div className="flex items-baseline gap-2">
      <span className="text-sm">{moment(value).format("MMM D, YYYY")}</span>
      <span className="text-xs text-muted-foreground">
        {moment(value).format("h:mm A")}
      </span>
    </div>
  </dd>
);

export const BotDetailsCard = ({ bot }: { bot: Bot }) => (
  <div className="p-4 sm:p-6 bg-background rounded-lg">
    <h2 className="text-sm font-medium mb-4">Bot Details</h2>
    <dl className="space-y-3 sm:space-y-4">
      {bot.config.symbol && (
        <DetailRow label="Symbol">
          <dd className="col-span-2 text-sm font-medium">
            {bot.config.symbol}
          </dd>
        </DetailRow>
      )}
      <DetailRow label="Type">
        <dd className="col-span-2">
          <code className="px-2 py-1 rounded bg-muted text-xs font-mono">
            {bot.config.type}
          </code>
        </dd>
      </DetailRow>
      <DetailRow label="Schedule">
        <dd className="col-span-2 text-sm">
          {bot.config.schedule || "Real-time"}
        </dd>
      </DetailRow>
      <DetailRow label="Created">
        <DateTimeCell value={bot.createdAt} />
      </DetailRow>
      <DetailRow label="Updated">
        <DateTimeCell value={bot.updatedAt} />
      </DetailRow>
    </dl>
  </div>
);

export const BotConfigCard = ({
  maskedConfig,
  onCopy,
}: {
  maskedConfig: unknown;
  onCopy: () => void;
}) => (
  <div className="p-4 sm:p-6 bg-background rounded-lg">
    <div className="flex justify-between items-center mb-4">
      <h2 className="text-sm font-medium">Configuration</h2>
      <Button variant="ghost" size="sm" onClick={onCopy} className="h-8 px-2">
        <Clipboard className="h-4 w-4 mr-1 sm:mr-2" />
        <span className="hidden sm:inline">Copy</span>
      </Button>
    </div>
    <div className="relative">
      <pre className="text-xs bg-muted/50 p-3 sm:p-4 rounded font-mono overflow-auto max-h-[300px] whitespace-pre-wrap break-all">
        {JSON.stringify(maskedConfig, null, 2)}
      </pre>
    </div>
    <div className="mt-4 flex items-start gap-2">
      <AlertTriangle className="h-4 w-4 text-amber-500 mt-0.5 flex-shrink-0" />
      <p className="text-xs text-muted-foreground leading-relaxed">
        API keys and secrets are hidden for security reasons.
      </p>
    </div>
  </div>
);
