"use client";

import React from "react";
import { useRouter } from "next/navigation";
import moment from "moment";
import { Button } from "@/components/ui/button";
import { Switch } from "@/components/ui/switch";
import {
  ArrowLeft,
  Clipboard,
  Loader2,
  Terminal,
  Trash2,
  AlertTriangle,
} from "lucide-react";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { BotDashboardLoader } from "@/components/bot/bot-dashboard-loader";
import { ConsoleInterface } from "@/components/bot/console-interface";
import {
  IntervalPicker,
  IntervalValue,
} from "@/components/bot/interval-picker";
import { RefreshSelector } from "@/components/bot/refresh-selector";
import { Bot as ApiBotType } from "@/lib/api/api-client";
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
  customBotId?: string;
}

interface MobileBotDetailProps {
  bot: Bot;
  botId: string;
  customBotId?: string;
  maskedConfig: Record<string, any>;
  logs: any[];
  logsLoading: boolean;
  refreshLogs: () => void;
  setDateFilter: (date: string | null) => void;
  setDateRangeFilter: (startDate: string, endDate: string) => void;
  exportLogs: () => void;
  connected?: boolean;
  lastUpdate?: Date | null;
  hasEarlierLogs?: boolean;
  loadingEarlier?: boolean;
  loadEarlierLogs?: () => void;
  hasMore?: boolean;
  loadMore?: () => void;
  loadingMore?: boolean;
  isUpdatingEnabled: boolean;
  isDeleting: boolean;
  onToggleEnabled: (enabled: boolean) => void;
  onDelete: () => void;
  onCopyConfig: () => void;
  onOpenUpdateModal: () => void;
  interval: IntervalValue;
  onIntervalChange: (value: IntervalValue) => void;
  showLive?: boolean;
  refreshInterval: number;
  onRefreshIntervalChange: (ms: number) => void;
}

export function MobileBotDetail({
  bot,
  botId,
  customBotId,
  maskedConfig,
  logs,
  logsLoading,
  refreshLogs,
  setDateFilter,
  setDateRangeFilter,
  exportLogs,
  connected,
  lastUpdate,
  hasEarlierLogs,
  loadingEarlier,
  loadEarlierLogs,
  hasMore,
  loadMore,
  loadingMore,
  isUpdatingEnabled,
  isDeleting,
  onToggleEnabled,
  onDelete,
  onCopyConfig,
  onOpenUpdateModal,
  interval,
  onIntervalChange,
  showLive,
  refreshInterval,
  onRefreshIntervalChange,
}: MobileBotDetailProps) {
  const router = useRouter();

  return (
    <div className="flex flex-col h-[calc(100vh-3rem)]">
      {/* Header with back arrow */}
      <div className="border-b p-3 flex items-center gap-3 flex-shrink-0">
        <Button
          variant="ghost"
          size="icon"
          className="h-8 w-8"
          onClick={() => router.push("/dashboard")}
        >
          <ArrowLeft className="h-4 w-4" />
        </Button>
        <div className="min-w-0 flex-1">
          <h1 className="text-sm font-medium truncate">{bot.config.name}</h1>
          <p className="text-xs text-muted-foreground font-mono">
            {bot.id.slice(-6)}
          </p>
        </div>
        <div className="flex items-center gap-1.5">
          <Switch
            checked={bot.config.enabled ?? true}
            onCheckedChange={onToggleEnabled}
            disabled={isUpdatingEnabled}
          />
          {isUpdatingEnabled && <Loader2 className="h-3 w-3 animate-spin" />}
        </div>
      </div>

      {/* Interval Picker + Refresh Selector */}
      <div className="px-3 py-2 border-b space-y-1.5">
        <IntervalPicker value={interval} onChange={onIntervalChange} showLive={showLive} />
        <RefreshSelector value={refreshInterval} onChange={onRefreshIntervalChange} />
      </div>

      {/* Tabbed content */}
      <Tabs defaultValue="dashboard" className="flex-1 flex flex-col min-h-0">
        <TabsList className="w-full rounded-none border-b bg-transparent h-10 flex-shrink-0">
          <TabsTrigger value="dashboard" className="flex-1 text-xs">
            Dashboard
          </TabsTrigger>
          <TabsTrigger value="console" className="flex-1 text-xs">
            Console
          </TabsTrigger>
          <TabsTrigger value="details" className="flex-1 text-xs">
            Details
          </TabsTrigger>
        </TabsList>

        <TabsContent value="dashboard" className="flex-1 m-0 overflow-auto">
          {bot.config.hasFrontend && customBotId ? (
            <BotDashboardLoader
              key={botId}
              botId={botId}
              customBotId={customBotId}
              version={bot.config.version}
              dateRange={{ start: interval.start, end: interval.end }}
              refreshInterval={refreshInterval}
              className=""
            />
          ) : (
            <div className="min-h-[300px] flex items-center justify-center text-muted-foreground bg-muted/20">
              <p className="text-sm">No dashboard configured for this bot</p>
            </div>
          )}
        </TabsContent>

        <TabsContent value="console" className="flex-1 m-0 overflow-auto">
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
            hasMore={hasMore}
            loadMore={loadMore}
            loadingMore={loadingMore}
            className="h-full"
            compact
          />
        </TabsContent>

        <TabsContent value="details" className="flex-1 m-0 overflow-auto">
          <div className="p-4 space-y-4">
            {/* Bot Details */}
            <div className="p-4 bg-background rounded-lg border">
              <h2 className="text-sm font-medium mb-3">Bot Details</h2>
              <dl className="space-y-3">
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
                  <dd className="col-span-2 text-sm">
                    {moment(bot.createdAt).format("MMM D, YYYY h:mm A")}
                  </dd>
                </div>
                <div className="grid grid-cols-3 gap-1">
                  <dt className="text-sm text-muted-foreground">Updated</dt>
                  <dd className="col-span-2 text-sm">
                    {moment(bot.updatedAt).format("MMM D, YYYY h:mm A")}
                  </dd>
                </div>
              </dl>
            </div>

            {/* Configuration */}
            <div className="p-4 bg-background rounded-lg border">
              <div className="flex justify-between items-center mb-3">
                <h2 className="text-sm font-medium">Configuration</h2>
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={onCopyConfig}
                  className="h-7 px-2"
                >
                  <Clipboard className="h-3.5 w-3.5 mr-1" />
                  <span className="text-xs">Copy</span>
                </Button>
              </div>
              <pre className="text-xs bg-muted/50 p-3 rounded font-mono overflow-auto max-h-[200px] whitespace-pre-wrap break-all">
                {JSON.stringify(maskedConfig, null, 2)}
              </pre>
              <div className="mt-3 flex items-start gap-2">
                <AlertTriangle className="h-3.5 w-3.5 text-amber-500 mt-0.5 flex-shrink-0" />
                <p className="text-xs text-muted-foreground">
                  API keys and secrets are hidden for security reasons.
                </p>
              </div>
            </div>

            {/* Actions */}
            <div className="space-y-2">
              <Button
                variant="outline"
                size="sm"
                onClick={onOpenUpdateModal}
                className="w-full"
              >
                <Terminal className="h-4 w-4 mr-2" />
                Update via CLI
              </Button>
              <AlertDialog>
                <AlertDialogTrigger asChild>
                  <Button
                    variant="outline"
                    size="sm"
                    className="w-full text-destructive border-destructive hover:bg-destructive/10"
                  >
                    <Trash2 className="h-4 w-4 mr-2" />
                    Delete Bot
                  </Button>
                </AlertDialogTrigger>
                <AlertDialogContent className="mx-4 max-w-md">
                  <AlertDialogHeader>
                    <AlertDialogTitle>Delete Bot</AlertDialogTitle>
                    <AlertDialogDescription>
                      Are you sure you want to delete this bot? This action
                      cannot be undone and all trading activity will immediately
                      cease.
                    </AlertDialogDescription>
                  </AlertDialogHeader>
                  <AlertDialogFooter className="flex-col-reverse gap-2 sm:flex-row">
                    <AlertDialogCancel className="w-full sm:w-auto">
                      Cancel
                    </AlertDialogCancel>
                    <AlertDialogAction
                      onClick={onDelete}
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
        </TabsContent>
      </Tabs>
    </div>
  );
}
