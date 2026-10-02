"use client";

import React from "react";
import { Copy, ExternalLink, Terminal } from "lucide-react";
import { Button } from "@/components/ui/button";
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
