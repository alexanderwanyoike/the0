"use client";

import React, { use, useEffect, useState } from "react";
import { useAuth } from "@/contexts/auth-context";
import DashboardLayout from "@/components/layouts/dashboard-layout";
import { useRouter } from "next/navigation";
import moment from "moment";
import { Button } from "@/components/ui/button";
import { Switch } from "@/components/ui/switch";
import {
  Clipboard,
  Loader2,
  Terminal,
  Trash2,
  AlertTriangle,
  Copy,
  ExternalLink,
} from "lucide-react";
import { useToast } from "@/hooks/use-toast";
import { withAuth } from "@/components/auth/with-auth";
import { BotDashboardLoader } from "@/components/bot/bot-dashboard-loader";
import { ConsoleInterface } from "@/components/bot/console-interface";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { useBotLogs } from "@/hooks/use-bot-logs";
import { BotService, Bot as ApiBotType } from "@/lib/api/api-client";
import { getErrorMessage } from "@/lib/axios";
import { cn } from "@/lib/utils";

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

// Extend the API Bot type with additional properties we need
interface Bot extends ApiBotType {
  userId?: string;
  user_id?: string;
  name?: string;
  version?: string;
  customBotId?: string;
}

interface BotDetailProps {
  params: Promise<{ botId: string }>;
}

/**
 * Responsive Bot Detail Page
 *
 * Mobile-first design with progressive enhancement:
 * - Collapsible sections on mobile
 * - Stacked layout on small screens
 * - Responsive chart sizing
 * - Touch-friendly interactions
 */
const BotDetail = ({ params }: BotDetailProps) => {
  const { botId } = use(params);
  const [bot, setBot] = useState<Bot | null>(null);
  const [loading, setLoading] = useState(true);
  const [_, setError] = useState<string | null>(null);
  const [isDeleting, setIsDeleting] = useState(false);
  const [isUpdatingEnabled, setIsUpdatingEnabled] = useState(false);
  const [isUpdateModalOpen, setIsUpdateModalOpen] = useState(false);
  const router = useRouter();
  const { toast } = useToast();
  const { user } = useAuth();

  // Console logs hook
  const {
    logs,
    loading: logsLoading,
    refresh: refreshLogs,
    setDateFilter,
    setDateRangeFilter,
    exportLogs,
  } = useBotLogs({
    botId,
    autoRefresh: true,
    refreshInterval: 60 * 1000, // 1 minute
  });

  useEffect(() => {
    const fetchBot = async () => {
      if (!botId || !user) return;
      setLoading(true);
      setError(null);
      try {
        const result = await BotService.getBot(botId);

        if (!result.success) {
          throw new Error(result.error.message || "Failed to fetch bot");
        }

        const botData = result.data;
        // Check authorization (API might use 'user_id' instead of 'userId')
        const botUserId = (botData as any).userId || (botData as any).user_id;
        if (botUserId !== user.id) throw new Error("Unauthorized access");

        // Add a default status if not present
        if (!botData.status) {
          botData.status = "running";
        }

        setBot(botData);
      } catch (error) {
        console.error("Error fetching bot:", error);
        const errorMessage =
          error instanceof Error ? error.message : "An unknown error occurred";
        setError(errorMessage);

        // Show toast for errors
        toast({
          title: "Error",
          description: `Failed to load bot: ${errorMessage}`,
          variant: "destructive",
        });

        if (
          error instanceof Error &&
          (error.message === "Bot not found" ||
            error.message === "Unauthorized access")
        ) {
          setTimeout(() => router.push("/dashboard"), 2000);
        }
      } finally {
        setLoading(false);
      }
    };

    fetchBot();
  }, [botId, user, router, toast]);

  // Copy config to clipboard with sensitive data masked
  const copyToClipboard = () => {
    if (!bot) return;

    // Create a deep copy of the config
    const configCopy = JSON.parse(JSON.stringify(bot.config));

    // Mask sensitive data before copying
    navigator.clipboard.writeText(JSON.stringify(configCopy, null, 2));
    toast({
      description: "Bot configuration copied to clipboard",
      duration: 2000,
    });
  };

  // Copy update command to clipboard
  const handleCopyUpdateCommand = () => {
    const command = `the0 bot update ${botId} config.json`;
    navigator.clipboard.writeText(command);
    toast({
      title: "Command Copied",
      description: "CLI update command copied to clipboard.",
    });
  };

  // Copy bot ID to clipboard
  const handleCopyBotId = () => {
    navigator.clipboard.writeText(botId);
    toast({
      title: "Bot ID Copied",
      description: "Bot ID copied to clipboard.",
    });
  };

  // Handle bot deletion
  const handleDeleteBot = async () => {
    if (!bot) return;
    setIsDeleting(true);

    try {
      const result = await BotService.deleteBot(botId);

      if (!result.success) {
        throw new Error(result.error.message || "Failed to delete bot");
      }

      toast({
        description: "Bot deleted successfully",
        duration: 2000,
      });

      // Redirect to dashboard list page
      router.push("/dashboard");
    } catch (error) {
      console.error("Error deleting bot:", error);
      toast({
        title: "Delete Failed",
        description:
          error instanceof Error ? error.message : "Failed to delete bot",
        variant: "destructive",
      });
      setIsDeleting(false);
    }
  };

  // Handle bot enable/disable toggle
  const handleToggleEnabled = async (enabled: boolean) => {
    if (!bot) return;
    setIsUpdatingEnabled(true);

    try {
      // Create updated config with the new enabled status
      const updatedConfig = {
        ...bot.config,
        enabled,
      };

      const result = await BotService.updateBot(botId, updatedConfig);

      if (result.success) {
        // Update local bot state
        setBot((prev) =>
          prev
            ? {
                ...prev,
                config: updatedConfig,
              }
            : null,
        );

        toast({
          description: `Bot ${enabled ? "enabled" : "disabled"} successfully. It may take a few moments to reflect the change.`,
          duration: 2000,
        });
      } else {
        throw new Error(result.error.message);
      }
    } catch (error) {
      console.error("Error updating bot enabled status:", error);
      toast({
        title: "Update Failed",
        description: getErrorMessage(error),
        variant: "destructive",
      });
    } finally {
      setIsUpdatingEnabled(false);
    }
  };

  // Get status badge color based on bot status
  const getStatusBadgeClass = (status: string) => {
    switch (status) {
      case "running":
        return "bg-green-100 text-green-700 dark:bg-green-900/30 dark:text-green-400";
      case "stopped":
        return "bg-red-100 text-red-700 dark:bg-red-900/30 dark:text-red-400";
      case "paused":
        return "bg-amber-100 text-amber-700 dark:bg-amber-900/30 dark:text-amber-400";
      case "restarting":
        return "bg-blue-100 text-blue-700 dark:bg-blue-900/30 dark:text-blue-400";
      default:
        return "bg-gray-100 text-gray-700 dark:bg-gray-900/30 dark:text-gray-400";
    }
  };

  // Create a filtered and masked version of config for display
  const getMaskedConfig = (config: Record<string, any>) => {
    const configCopy = JSON.parse(JSON.stringify(config));

    // List of sensitive field patterns to completely remove or mask
    const sensitivePatterns = [
      /api[_-]?key/i,
      /secret[_-]?key/i,
      /api[_-]?secret/i,
      /password/i,
      /token/i,
      /key$/i,
      /private[_-]?key/i,
      /access[_-]?token/i,
      /refresh[_-]?token/i,
      /auth[_-]?token/i,
      /bearer[_-]?token/i,
      /exchange.*key/i,
      /exchange.*secret/i,
      /credential/i,
      /passphrase/i,
      /pin/i,
      /code/i,
    ];

    const filterSensitiveData = (obj: any): any => {
      if (!obj || typeof obj !== "object") return obj;

      const filtered: any = Array.isArray(obj) ? [] : {};

      Object.keys(obj).forEach((key) => {
        const isSensitive = sensitivePatterns.some((pattern) =>
          pattern.test(key),
        );

        if (isSensitive) {
          // Skip sensitive fields entirely - don't include them in the display
          return;
        } else if (typeof obj[key] === "object" && obj[key] !== null) {
          filtered[key] = filterSensitiveData(obj[key]);
        } else {
          filtered[key] = obj[key];
        }
      });

      return filtered;
    };

    return filterSensitiveData(configCopy);
  };

  if (loading) {
    return (
      <DashboardLayout>
        <div className="flex min-h-[50vh] items-center justify-center">
          <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
        </div>
      </DashboardLayout>
    );
  }

  if (!bot) return null;

  const maskedConfig = getMaskedConfig(bot.config);

  // Get customBotId from bot instance (this is the unique ID for the specific custom bot version)
  const customBotId = bot.customBotId;

  return (
    <DashboardLayout>
      <div className="min-h-full flex flex-col gap-4">
        {/* Header */}
        <div className="sticky top-0 z-10 border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60">
          <div className="p-4 lg:px-6 lg:py-4">
            {/* Desktop Layout */}
            <div className="hidden md:flex md:items-center md:justify-between">
              <div className="flex items-center space-x-4">
                <h1 className="text-lg font-medium">{bot.config.name}</h1>
              </div>
              <div className="flex items-center gap-2">
                <p className="text-sm text-muted-foreground font-mono">
                  {bot.id.slice(-6)}
                </p>
                <div className="flex items-center gap-2">
                  <div className="flex items-center gap-2">
                    <Switch
                      checked={bot.config.enabled ?? true}
                      onCheckedChange={handleToggleEnabled}
                      disabled={isUpdatingEnabled}
                    />
                    <span className="text-sm text-muted-foreground">
                      {isUpdatingEnabled ? (
                        <Loader2 className="h-4 w-4 animate-spin" />
                      ) : (bot.config.enabled ?? true) ? (
                        "Enabled"
                      ) : (
                        "Disabled"
                      )}
                    </span>
                  </div>
                  <Button
                    variant="outline"
                    size="sm"
                    onClick={() => setIsUpdateModalOpen(true)}
                  >
                    <Terminal className="h-4 w-4 mr-2" />
                    Update via CLI
                  </Button>
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
                          Are you sure you want to delete this bot? This action
                          cannot be undone and all trading activity will
                          immediately cease.
                        </AlertDialogDescription>
                      </AlertDialogHeader>
                      <AlertDialogFooter>
                        <AlertDialogCancel>Cancel</AlertDialogCancel>
                        <AlertDialogAction
                          onClick={handleDeleteBot}
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
                </div>
              </div>
            </div>

            {/* Mobile Layout */}
            <div className="md:hidden">
              <div className="space-y-3 sm:space-y-0 sm:flex sm:items-center sm:justify-between">
                <div className="space-y-2 sm:space-y-0 sm:flex sm:items-center sm:space-x-4">
                  <h1 className="text-lg font-medium truncate pr-2">
                    {bot.config.name}
                  </h1>
                </div>
                <div className="hidden xs:block">
                  <p className="text-xs text-muted-foreground font-mono truncate">
                    {bot.id.slice(-6)}
                  </p>
                </div>
              </div>

              <div className="space-y-3 mt-3">
                <div className="flex items-center justify-center gap-2 p-2 border rounded-md bg-muted/20">
                  <Switch
                    checked={bot.config.enabled ?? true}
                    onCheckedChange={handleToggleEnabled}
                    disabled={isUpdatingEnabled}
                  />
                  <span className="text-sm text-muted-foreground">
                    {isUpdatingEnabled ? (
                      <Loader2 className="h-4 w-4 animate-spin" />
                    ) : (bot.config.enabled ?? true) ? (
                      "Enabled"
                    ) : (
                      "Disabled"
                    )}
                  </span>
                </div>
                <div className="flex flex-row xs:flex-row gap-2">
                  <Button
                    variant="outline"
                    size="sm"
                    onClick={() => setIsUpdateModalOpen(true)}
                    className="flex-1 xs:flex-none py-1"
                  >
                    <Terminal className="h-4 w-4 mr-2" />
                    Update via CLI
                  </Button>
                  <AlertDialog>
                    <AlertDialogTrigger asChild>
                      <Button
                        variant="outline"
                        size="sm"
                        className="flex-1 xs:flex-none text-destructive border-destructive hover:bg-destructive/10 py-1"
                      >
                        <Trash2 className="h-4 w-4 mr-2" />
                        Delete
                      </Button>
                    </AlertDialogTrigger>
                    <AlertDialogContent className="mx-4 max-w-md">
                      <AlertDialogHeader>
                        <AlertDialogTitle>Delete Bot</AlertDialogTitle>
                        <AlertDialogDescription>
                          Are you sure you want to delete this bot? This action
                          cannot be undone and all trading activity will
                          immediately cease.
                        </AlertDialogDescription>
                      </AlertDialogHeader>
                      <AlertDialogFooter className="flex-col-reverse gap-2 sm:flex-row">
                        <AlertDialogCancel className="w-full sm:w-auto">
                          Cancel
                        </AlertDialogCancel>
                        <AlertDialogAction
                          onClick={handleDeleteBot}
                          className="w-full sm:w-auto bg-destructive text-destructive-foreground hover:bg-destructive/90"
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
                </div>
              </div>
            </div>
          </div>
        </div>

        {/* Main Content - Dashboard and Console side by side */}
        <div className="flex flex-row p-4 gap-4 max-h-[90vh]">
          {/* Dashboard Area - 60% */}
          <div className="w-[60%] rounded-lg border overflow-auto">
            {bot.config.hasFrontend && customBotId ? (
              <BotDashboardLoader
                botId={botId}
                customBotId={customBotId}
                className=""
              />
            ) : (
              <div className="min-h-[400px] flex items-center justify-center text-muted-foreground bg-muted/20">
                <p>No dashboard configured for this bot</p>
              </div>
            )}
          </div>

          {/* Console Panel - 40% */}
          <div className="w-[40%] flex flex-col border rounded-lg bg-background overflow-hidden">
            {/* Console Header */}
            <div className="h-10 px-4 flex items-center justify-between text-sm font-medium border-b bg-muted/30 flex-shrink-0">
              <div className="flex items-center gap-2">
                <Terminal className="h-4 w-4" />
                <span>Console</span>
                {logs.length > 0 && (
                  <span className="text-xs text-muted-foreground">
                    ({logs.length})
                  </span>
                )}
              </div>
            </div>

            {/* Console Content - scrolls internally */}
            <div className="flex-1 min-h-0 overflow-auto">
              <ConsoleInterface
                botId={botId}
                logs={logs}
                loading={logsLoading}
                onRefresh={refreshLogs}
                onDateChange={setDateFilter}
                onDateRangeChange={setDateRangeFilter}
                onExport={exportLogs}
                className="h-full"
                compact
              />
            </div>
          </div>
        </div>

        {/* Bot Details */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-4 p-4 bg-muted/30">
          {/* Bot Details Section */}
          <div className="p-4 sm:p-6 bg-background rounded-lg">
            <h2 className="text-sm font-medium mb-4">Bot Details</h2>
            <dl className="space-y-3 sm:space-y-4">
              {bot.config.symbol && (
                <div className="grid grid-cols-3 gap-1">
                  <dt className="text-sm text-muted-foreground">Symbol</dt>
                  <dd className="col-span-2 text-sm font-medium">
                    {bot.config.symbol}
                  </dd>
                </div>
              )}
              <div className="grid grid-cols-3 gap-1">
                <dt className="text-sm text-muted-foreground">Type</dt>
                <dd className="col-span-2">
                  <code className="px-2 py-1 rounded bg-muted text-xs font-mono">
                    {bot.config.type}
                  </code>
                </dd>
              </div>
              <div className="grid grid-cols-3 gap-1">
                <dt className="text-sm text-muted-foreground">Schedule</dt>
                <dd className="col-span-2 text-sm">
                  {bot.config.schedule || "Real-time"}
                </dd>
              </div>
              <div className="grid grid-cols-3 gap-1">
                <dt className="text-sm text-muted-foreground">Created</dt>
                <dd className="col-span-2">
                  <div className="flex items-baseline gap-2">
                    <span className="text-sm">
                      {moment(bot.createdAt).format("MMM D, YYYY")}
                    </span>
                    <span className="text-xs text-muted-foreground">
                      {moment(bot.createdAt).format("h:mm A")}
                    </span>
                  </div>
                </dd>
              </div>
              <div className="grid grid-cols-3 gap-1">
                <dt className="text-sm text-muted-foreground">Updated</dt>
                <dd className="col-span-2">
                  <div className="flex items-baseline gap-2">
                    <span className="text-sm">
                      {moment(bot.updatedAt).format("MMM D, YYYY")}
                    </span>
                    <span className="text-xs text-muted-foreground">
                      {moment(bot.updatedAt).format("h:mm A")}
                    </span>
                  </div>
                </dd>
              </div>
            </dl>
          </div>

          {/* Configuration Section */}
          <div className="p-4 sm:p-6 bg-background rounded-lg">
            <div className="flex justify-between items-center mb-4">
              <h2 className="text-sm font-medium">Configuration</h2>
              <Button
                variant="ghost"
                size="sm"
                onClick={copyToClipboard}
                className="h-8 px-2"
              >
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
        </div>
      </div>

      {/* CLI Update Modal */}
      <Dialog open={isUpdateModalOpen} onOpenChange={setIsUpdateModalOpen}>
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
            {/* Bot ID */}
            <div className="space-y-2">
              <Label className="text-sm font-medium">Bot ID</Label>
              <div className="flex gap-2">
                <Input
                  value={botId}
                  readOnly
                  className="font-mono text-sm bg-muted"
                />
                <Button
                  variant="outline"
                  size="sm"
                  onClick={handleCopyBotId}
                  className="gap-2 shrink-0"
                >
                  <Copy className="h-4 w-4" />
                  Copy
                </Button>
              </div>
            </div>

            {/* Update Command */}
            <div className="space-y-2">
              <Label className="text-sm font-medium">CLI Update Command</Label>
              <div className="flex gap-2">
                <Input
                  value={`the0 bot update ${botId} config.json`}
                  readOnly
                  className="font-mono text-sm bg-muted"
                />
                <Button
                  variant="outline"
                  size="sm"
                  onClick={handleCopyUpdateCommand}
                  className="gap-2 shrink-0"
                >
                  <Copy className="h-4 w-4" />
                  Copy
                </Button>
              </div>
              <p className="text-xs text-muted-foreground">
                Run this command in your terminal where the the0 CLI is
                installed.
              </p>
            </div>

            {/* Instructions */}
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

            {/* Helpful CLI Commands */}
            <div className="space-y-2">
              <Label className="text-sm font-medium">
                Helpful CLI Commands
              </Label>
              <div className="space-y-2 text-xs font-mono bg-muted p-4 rounded-lg">
                <p className="text-muted-foreground"># View bot details</p>
                <p>the0 bot list</p>
                <p className="text-muted-foreground mt-2"># View bot logs</p>
                <p>the0 bot logs {botId}</p>
                <p className="text-muted-foreground mt-2"># Delete bot</p>
                <p>the0 bot delete {botId}</p>
              </div>
            </div>

            {/* Documentation Link */}
            <div className="flex items-center gap-2 text-sm">
              <ExternalLink className="h-4 w-4 text-muted-foreground" />
              <a
                href="https://docs.the0.dev/the0-CLI/bot-commands"
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
    </DashboardLayout>
  );
};

export default withAuth(BotDetail);
