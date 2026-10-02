"use client";

import React from "react";
import moment from "moment";
import {
  AlertTriangle,
  Clipboard,
  Copy,
  ExternalLink,
  Loader2,
  Terminal,
  Trash2,
} from "lucide-react";
import { Button } from "@/components/ui/button";
import { Switch } from "@/components/ui/switch";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { useToast } from "@/hooks/use-toast";
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
import type { DetailBot as Bot } from "./use-bot-detail";

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

const CopyableField = ({
  value,
  onCopy,
}: {
  value: string;
  onCopy: () => void;
}) => (
  <div className="flex gap-2">
    <Input value={value} readOnly className="font-mono text-sm bg-muted" />
    <Button
      variant="outline"
      size="sm"
      onClick={onCopy}
      className="gap-2 shrink-0"
    >
      <Copy className="h-4 w-4" />
      Copy
    </Button>
  </div>
);

interface CliUpdateDialogProps {
  botId: string;
  open: boolean;
  onOpenChange: (open: boolean) => void;
}

export function CliUpdateDialog({
  botId,
  open,
  onOpenChange,
}: CliUpdateDialogProps) {
  const { toast } = useToast();
  const updateCommand = `the0 bot update ${botId} config.json`;

  const handleCopyUpdateCommand = () => {
    navigator.clipboard.writeText(updateCommand);
    toast({
      title: "Command Copied",
      description: "CLI update command copied to clipboard.",
    });
  };

  const handleCopyBotId = () => {
    navigator.clipboard.writeText(botId);
    toast({
      title: "Bot ID Copied",
      description: "Bot ID copied to clipboard.",
    });
  };

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="max-w-2xl max-h-[90vh] overflow-y-auto">
        <DialogHeader>
          <DialogTitle className="flex items-center gap-2">
            <Terminal className="h-5 w-5" />
            Update Bot via CLI
          </DialogTitle>
          <DialogDescription>
            Use the the0 CLI to update your bot configuration.
          </DialogDescription>
        </DialogHeader>
        <div className="space-y-6">
          <div className="space-y-2">
            <Label className="text-sm font-medium">Bot ID</Label>
            <CopyableField value={botId} onCopy={handleCopyBotId} />
          </div>
          <div className="space-y-2">
            <Label className="text-sm font-medium">CLI Update Command</Label>
            <CopyableField
              value={updateCommand}
              onCopy={handleCopyUpdateCommand}
            />
            <p className="text-xs text-muted-foreground">
              Run this command in your terminal where the the0 CLI is installed.
            </p>
          </div>
          <div className="p-4 bg-blue-50 dark:bg-blue-950/20 rounded-lg border border-blue-200 dark:border-blue-800">
            <div className="space-y-3">
              <p className="text-sm text-blue-800 dark:text-blue-200 font-medium">
                How to update your bot:
              </p>
              <ol className="text-xs text-blue-700 dark:text-blue-300 space-y-2 list-decimal list-inside">
                <li>
                  Create a{" "}
                  <code className="px-1 bg-blue-100 dark:bg-blue-900 rounded">
                    config.json
                  </code>{" "}
                  file with your updated configuration
                </li>
                <li>Run the update command above in your terminal</li>
                <li>The bot will be updated with the new configuration</li>
              </ol>
            </div>
          </div>
          <div className="space-y-2">
            <Label className="text-sm font-medium">Helpful CLI Commands</Label>
            <div className="space-y-2 text-xs font-mono bg-muted p-4 rounded-lg">
              <p className="text-muted-foreground"># View bot details</p>
              <p>the0 bot list</p>
              <p className="text-muted-foreground mt-2"># View bot logs</p>
              <p>the0 bot logs {botId}</p>
              <p className="text-muted-foreground mt-2"># Delete bot</p>
              <p>the0 bot delete {botId}</p>
            </div>
          </div>
          <div className="flex items-center gap-2 text-sm">
            <ExternalLink className="h-4 w-4 text-muted-foreground" />
            <a
              href="https://docs.the0.app/the0-CLI/bot-commands"
              target="_blank"
              rel="noopener noreferrer"
              className="text-primary hover:underline"
            >
              View CLI Documentation
            </a>
          </div>
        </div>
      </DialogContent>
    </Dialog>
  );
}
