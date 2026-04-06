"use client";

import React, { useEffect, useMemo, useRef, useState } from "react";
import { useAuth } from "@/contexts/auth-context";
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
import { BotDashboardLoader } from "@/components/bot/bot-dashboard-loader";
import { ConsoleInterface } from "@/components/bot/console-interface";
import {
  IntervalPicker,
  IntervalValue,
  LIVE_INTERVAL,
  DEFAULT_INTERVAL,
} from "@/components/bot/interval-picker";
import { RefreshSelector } from "@/components/bot/refresh-selector";
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
import { useBotLogsStream } from "@/hooks/use-bot-logs-stream";
import { BotService, Bot as ApiBotType } from "@/lib/api/api-client";
import { getErrorMessage } from "@/lib/axios";
import { shouldUseLogStreaming } from "@/lib/bot-utils";
import { useDashboardBots } from "@/contexts/dashboard-bots-context";
import { useMediaQuery } from "@/hooks/use-media-query";
import { MobileBotDetail } from "./mobile-bot-detail";
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

interface Bot extends ApiBotType {
  userId?: string;
  user_id?: string;
  name?: string;
  version?: string;
  customBotId?: string;
}

interface BotDetailPanelProps {
  botId: string;
}

export function BotDetailPanel({ botId }: BotDetailPanelProps) {
  const [bot, setBot] = useState<Bot | null>(null);
  const [loading, setLoading] = useState(true);
  const [isDeleting, setIsDeleting] = useState(false);
  const [isUpdatingEnabled, setIsUpdatingEnabled] = useState(false);
  const [isUpdateModalOpen, setIsUpdateModalOpen] = useState(false);
  const router = useRouter();
  const { toast } = useToast();
  const { user } = useAuth();
  const { removeBotFromList, bots } = useDashboardBots();
  const mediaQuery = useMediaQuery("(min-width: 1280px)");
  const isMobile = mediaQuery === null ? null : !mediaQuery;

  // Configurable refresh rate for polling (console + dashboard)
  const [refreshInterval, setRefreshInterval] = useState(30000);

  // Console logs: use SSE streaming for realtime bots, REST polling for scheduled.
  const useStreaming = shouldUseLogStreaming(bot);

  // Interval picker state (shared by dashboard + console)
  // Realtime bots default to live mode; scheduled bots default to 1d range.
  // Note: bot is null at mount, so useStreaming is initially false. The
  // useEffect below corrects the interval once the bot loads.
  const [interval, setInterval_] = useState<IntervalValue>(
    useStreaming ? LIVE_INTERVAL : DEFAULT_INTERVAL,
  );
  const streamingInitialized = useRef(false);

  // Reset interval default when bot changes or streaming status resolves
  useEffect(() => {
    streamingInitialized.current = false;
  }, [botId]);

  useEffect(() => {
    if (!streamingInitialized.current && bot) {
      streamingInitialized.current = true;
      setInterval_(useStreaming ? LIVE_INTERVAL : DEFAULT_INTERVAL);
    }
  }, [useStreaming, bot]);

  const hookBotId = bot !== null ? botId : "";

  const streamHook = useBotLogsStream({
    botId: useStreaming ? hookBotId : "",
    refreshInterval: refreshInterval || undefined,
  });

  const pollingHook = useBotLogs({
    botId: useStreaming ? "" : hookBotId,
    autoRefresh: !useStreaming && bot !== null && refreshInterval > 0,
    refreshInterval: refreshInterval || undefined,
  });

  const activeHook = useStreaming ? streamHook : pollingHook;

  const handleIntervalChange = (val: IntervalValue) => {
    setInterval_(val);
    if (val.type === "live") {
      // Clear date filter so SSE streams all logs
      activeHook.setDateFilter(null);
    } else {
      activeHook.setDateRangeFilter(val.start, val.end);
    }
  };

  const {
    logs,
    loading: logsLoading,
    refresh: refreshLogs,
    setDateFilter,
    setDateRangeFilter,
    exportLogs,
  } = activeHook;

  const connected = useStreaming ? streamHook.connected : undefined;
  const lastUpdate = useStreaming ? streamHook.lastUpdate : undefined;
  const hasEarlierLogs = useStreaming ? streamHook.hasEarlierLogs : undefined;
  const loadingEarlier = useStreaming ? streamHook.loadingEarlier : undefined;
  const loadEarlierLogs = useStreaming ? streamHook.loadEarlierLogs : undefined;

  // Pagination: only relevant for REST polling (not SSE streaming)
  const hasMoreLogs = !useStreaming ? pollingHook.hasMore : undefined;
  const loadMoreLogs = !useStreaming ? pollingHook.loadMore : undefined;

  useEffect(() => {
    const fetchBot = async () => {
      if (!botId || !user) return;
      setLoading(true);
      try {
        const result = await BotService.getBot(botId);
        if (!result.success) {
          throw new Error(result.error.message || "Failed to fetch bot");
        }
        const botData = result.data;
        const botUserId = (botData as any).userId || (botData as any).user_id;
        if (botUserId !== user.id) throw new Error("Unauthorized access");
        setBot(botData);
      } catch (error) {
        console.error("Error fetching bot:", error);
        toast({
          title: "Error",
          description: `Failed to load bot: ${error instanceof Error ? error.message : "Unknown error"}`,
          variant: "destructive",
        });
        if (
          error instanceof Error &&
          (error.message === "Bot not found" ||
            error.message === "Unauthorized access")
        ) {
          const timeout = setTimeout(() => router.push("/dashboard"), 2000);
          return () => clearTimeout(timeout);
        }
      } finally {
        setLoading(false);
      }
    };
    fetchBot();
  }, [botId, user, router, toast]);

  const copyToClipboard = () => {
    if (!bot) return;
    const masked = getMaskedConfig(bot.config);
    navigator.clipboard.writeText(JSON.stringify(masked, null, 2));
    toast({
      description: "Bot configuration copied to clipboard",
      duration: 2000,
    });
  };

  const handleCopyUpdateCommand = () => {
    navigator.clipboard.writeText(`the0 bot update ${botId} config.json`);
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

  const handleDeleteBot = async () => {
    if (!bot) return;
    setIsDeleting(true);
    try {
      const result = await BotService.deleteBot(botId);
      if (!result.success) {
        throw new Error(result.error.message || "Failed to delete bot");
      }
      toast({ description: "Bot deleted successfully", duration: 2000 });
      removeBotFromList(botId);
      // Navigate to next bot or dashboard
      const remaining = bots.filter((b) => b.id !== botId);
      if (remaining.length > 0) {
        router.replace(`/dashboard/${remaining[0].id}`);
      } else {
        router.replace("/dashboard");
      }
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

  const handleToggleEnabled = async (enabled: boolean) => {
    if (!bot) return;
    setIsUpdatingEnabled(true);
    try {
      const updatedConfig = { ...bot.config, enabled };
      const result = await BotService.updateBot(botId, updatedConfig);
      if (result.success) {
        setBot((prev) => (prev ? { ...prev, config: updatedConfig } : null));
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

  const getMaskedConfig = (config: Record<string, any>) => {
    const configCopy = JSON.parse(JSON.stringify(config));
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
    ];
    const filterSensitiveData = (obj: any): any => {
      if (!obj || typeof obj !== "object") return obj;
      const filtered: any = Array.isArray(obj) ? [] : {};
      Object.keys(obj).forEach((key) => {
        if (sensitivePatterns.some((p) => p.test(key))) return;
        if (typeof obj[key] === "object" && obj[key] !== null) {
          filtered[key] = filterSensitiveData(obj[key]);
        } else {
          filtered[key] = obj[key];
        }
      });
      return filtered;
    };
    return filterSensitiveData(configCopy);
  };

  // Memoize the masked config to avoid JSON.parse(JSON.stringify()) on every render
  const maskedConfig = useMemo(
    () => (bot ? getMaskedConfig(bot.config) : null),
    [bot?.config],
  );

  if (loading) {
    return (
      <div className="flex min-h-[50vh] items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  if (!bot) return null;

  const customBotId = bot.customBotId;

  // Shared CLI update modal (used by both mobile and desktop)
  const cliUpdateModal = (
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

  // Wait for media query to resolve
  if (isMobile === null) {
    return (
      <div className="flex min-h-[50vh] items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  // Mobile: render tabbed layout
  if (isMobile) {
    return (
      <>
        <MobileBotDetail
          bot={bot}
          botId={botId}
          customBotId={customBotId}
          maskedConfig={maskedConfig}
          logs={logs}
          logsLoading={logsLoading}
          refreshLogs={refreshLogs}
          setDateFilter={setDateFilter}
          setDateRangeFilter={setDateRangeFilter}
          exportLogs={exportLogs}
          connected={connected}
          lastUpdate={lastUpdate}
          hasEarlierLogs={hasEarlierLogs}
          loadingEarlier={loadingEarlier}
          loadEarlierLogs={loadEarlierLogs}
          hasMore={hasMoreLogs}
          loadMore={loadMoreLogs}
          loadingMore={!useStreaming ? logsLoading : undefined}
          isUpdatingEnabled={isUpdatingEnabled}
          isDeleting={isDeleting}
          onToggleEnabled={handleToggleEnabled}
          onDelete={handleDeleteBot}
          onCopyConfig={copyToClipboard}
          onOpenUpdateModal={() => setIsUpdateModalOpen(true)}
          interval={interval}
          onIntervalChange={handleIntervalChange}
          showLive={useStreaming}
          refreshInterval={refreshInterval}
          onRefreshIntervalChange={setRefreshInterval}
        />
        {cliUpdateModal}
      </>
    );
  }

  // Desktop/tablet: side-by-side layout
  return (
    <>
      <div className="min-h-full flex flex-col gap-4">
        {/* Header */}
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
          </div>
        </div>

        {/* Interval Picker + Refresh Selector */}
        <div className="px-4 lg:px-6 flex flex-wrap items-center gap-4">
          <IntervalPicker value={interval} onChange={handleIntervalChange} showLive={useStreaming} />
          <RefreshSelector value={refreshInterval} onChange={setRefreshInterval} />
        </div>

        {/* Main Content - Dashboard and Console side by side */}
        <div
          className="flex flex-row px-4 gap-4 flex-1 min-h-0"
          style={{ height: "calc(100vh - 14rem)" }}
        >
          {/* Dashboard Area - 60% */}
          <div className="w-[60%] rounded-lg border overflow-auto">
            {bot.config.hasFrontend && customBotId ? (
              <BotDashboardLoader
                key={botId}
                botId={botId}
                customBotId={customBotId}
                version={bot.config.version}
                dateRange={interval.type === "range" ? { start: interval.start, end: interval.end } : undefined}
                refreshInterval={refreshInterval}
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
            <div className="flex-1 min-h-0 overflow-auto">
              <ConsoleInterface
                botId={botId}
                logs={logs}
                loading={logsLoading}
                onRefresh={refreshLogs}
                onDateChange={setDateFilter}
                onDateRangeChange={setDateRangeFilter}
                onExport={exportLogs}
                connected={connected}
                lastUpdate={lastUpdate ?? null}
                hasEarlierLogs={hasEarlierLogs}
                loadingEarlier={loadingEarlier}
                onLoadEarlier={loadEarlierLogs}
                hasMore={hasMoreLogs}
                loadMore={loadMoreLogs}
                loadingMore={!useStreaming ? logsLoading : undefined}
                className="h-full"
                compact
              />
            </div>
          </div>
        </div>

        {/* Bot Details */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-4 p-4 bg-muted/30">
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

      {cliUpdateModal}
    </>
  );
}
