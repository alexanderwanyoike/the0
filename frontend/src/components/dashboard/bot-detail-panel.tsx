"use client";

import React, { useState } from "react";
import { Loader2, Terminal } from "lucide-react";
import { BotDashboardLoader } from "@/components/bot/bot-dashboard-loader";
import { ConsoleInterface } from "@/components/bot/console-interface";
import { IntervalPicker } from "@/components/bot/interval-picker";
import { ConnectionStatusIndicator } from "@/components/bot/console-interface";
import { useMediaQuery } from "@/hooks/use-media-query";
import { MobileBotDetail } from "./mobile-bot-detail";
import { CliUpdateDialog } from "./bot-detail/cli-update-dialog";
import {
  BotConfigCard,
  BotDetailHeader,
  BotDetailsCard,
} from "./bot-detail/bot-detail-sections";
import { useBotDetailLogs } from "./bot-detail/use-bot-detail-logs";
import { DetailBot, useOwnedBot } from "./bot-detail/use-owned-bot";

interface BotDetailPanelProps {
  botId: string;
}

const CenteredSpinner = () => (
  <div className="flex min-h-[50vh] items-center justify-center">
    <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
  </div>
);

type DetailLogs = ReturnType<typeof useBotDetailLogs>;

const DashboardArea = ({
  bot,
  botId,
  logs,
}: {
  bot: DetailBot;
  botId: string;
  logs: DetailLogs;
}) => (
  <div className="w-[60%] rounded-lg border overflow-auto">
    {bot.config.hasFrontend && bot.customBotId ? (
      <BotDashboardLoader
        key={botId}
        botId={botId}
        customBotId={bot.customBotId}
        version={bot.config.version}
        dateRange={logs.dashboardDateRange}
        latest={logs.interval.type === "latest"}
        streaming={logs.useStreaming}
        className=""
      />
    ) : (
      <div className="min-h-[400px] flex items-center justify-center text-muted-foreground bg-muted/20">
        <p>No dashboard configured for this bot</p>
      </div>
    )}
  </div>
);

const ConsolePanel = ({ botId, logs }: { botId: string; logs: DetailLogs }) => (
  <div className="w-[40%] flex flex-col border rounded-lg bg-background overflow-hidden">
    <div className="h-10 px-4 flex items-center justify-between text-sm font-medium border-b bg-muted/30 flex-shrink-0">
      <div className="flex items-center gap-2">
        <Terminal className="h-4 w-4" />
        <span>Console</span>
        {logs.logs.length > 0 && (
          <span className="text-xs text-muted-foreground">
            ({logs.logs.length})
          </span>
        )}
      </div>
    </div>
    <div className="flex-1 min-h-0 overflow-auto">
      <ConsoleInterface
        botId={botId}
        logs={logs.logs}
        loading={logs.logsLoading}
        onRefresh={logs.refreshLogs}
        onDateChange={logs.setDateFilter}
        onDateRangeChange={logs.setDateRangeFilter}
        onExport={logs.exportLogs}
        connected={logs.connected}
        lastUpdate={logs.lastUpdate ?? null}
        hasEarlierLogs={logs.hasEarlierLogs}
        loadingEarlier={logs.loadingEarlier}
        onLoadEarlier={logs.loadEarlierLogs}
        hasMore={logs.hasMoreLogs}
        loadMore={logs.loadMoreLogs}
        loadingMore={logs.loadingMore}
        sort={logs.sortOrder}
        onSortChange={logs.handleSortChange}
        className="h-full"
        compact
      />
    </div>
  </div>
);

export function BotDetailPanel({ botId }: BotDetailPanelProps) {
  const owned = useOwnedBot(botId);
  const { bot } = owned;
  const [isUpdateModalOpen, setIsUpdateModalOpen] = useState(false);
  const mediaQuery = useMediaQuery("(min-width: 1280px)");
  const isMobile = mediaQuery === null ? null : !mediaQuery;
  const logs = useBotDetailLogs(botId, bot);

  if (owned.loading) return <CenteredSpinner />;
  if (!bot) return null;

  const cliUpdateModal = (
    <CliUpdateDialog
      botId={botId}
      open={isUpdateModalOpen}
      onOpenChange={setIsUpdateModalOpen}
    />
  );

  if (isMobile === null) return <CenteredSpinner />;

  if (isMobile) {
    return (
      <>
        <MobileBotDetail
          bot={bot}
          botId={botId}
          customBotId={bot.customBotId}
          maskedConfig={owned.maskedConfig}
          logs={logs.logs}
          logsLoading={logs.logsLoading}
          refreshLogs={logs.refreshLogs}
          setDateFilter={logs.setDateFilter}
          setDateRangeFilter={logs.setDateRangeFilter}
          exportLogs={logs.exportLogs}
          connected={logs.connected}
          lastUpdate={logs.lastUpdate}
          hasEarlierLogs={logs.hasEarlierLogs}
          loadingEarlier={logs.loadingEarlier}
          loadEarlierLogs={logs.loadEarlierLogs}
          hasMore={logs.hasMoreLogs}
          loadMore={logs.loadMoreLogs}
          loadingMore={logs.loadingMore}
          isUpdatingEnabled={owned.isUpdatingEnabled}
          isDeleting={owned.isDeleting}
          onToggleEnabled={owned.toggleEnabled}
          onDelete={owned.deleteBot}
          onCopyConfig={owned.copyConfig}
          onOpenUpdateModal={() => setIsUpdateModalOpen(true)}
          interval={logs.interval}
          onIntervalChange={logs.handleIntervalChange}
          showLive={!logs.scheduled}
          showLatest={logs.scheduled}
          streaming={logs.useStreaming}
          sort={logs.sortOrder}
          onSortChange={logs.handleSortChange}
        />
        {cliUpdateModal}
      </>
    );
  }

  return (
    <>
      <div className="min-h-full flex flex-col gap-4">
        <BotDetailHeader
          bot={bot}
          isUpdatingEnabled={owned.isUpdatingEnabled}
          isDeleting={owned.isDeleting}
          onToggleEnabled={owned.toggleEnabled}
          onOpenUpdateModal={() => setIsUpdateModalOpen(true)}
          onDelete={owned.deleteBot}
        />

        <div className="px-4 lg:px-6 flex flex-wrap items-center gap-4">
          <IntervalPicker
            value={logs.interval}
            onChange={logs.handleIntervalChange}
            showLive={!logs.scheduled}
            showLatest={logs.scheduled}
          />
          <ConnectionStatusIndicator
            connected={logs.connected}
            lastUpdate={logs.lastUpdate}
          />
        </div>

        <div
          className="flex flex-row px-4 gap-4 flex-1 min-h-0"
          style={{ height: "calc(100vh - 14rem)" }}
        >
          <DashboardArea bot={bot} botId={botId} logs={logs} />
          <ConsolePanel botId={botId} logs={logs} />
        </div>

        <div className="grid grid-cols-1 lg:grid-cols-2 gap-4 p-4 bg-muted/30">
          <BotDetailsCard bot={bot} />
          <BotConfigCard
            maskedConfig={owned.maskedConfig}
            onCopy={owned.copyConfig}
          />
        </div>
      </div>

      {cliUpdateModal}
    </>
  );
}
