"use client";

import React from "react";
import { useRouter } from "next/navigation";
import { ArrowLeft, Bot, BookOpen, Info } from "lucide-react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { SchemaDisplay } from "@/components/custom-bots/schema-display";
import { ReadmeComponent } from "@/components/custom-bots/readme-component";
import { BotInfo } from "@/components/custom-bots/bot-info";
import { VersionHistory } from "@/components/custom-bots/version-history";
import { ActionButtons } from "@/components/custom-bots/action-buttons";
import { StatusHeader } from "@/components/custom-bots/status-header";
import {
  CustomBotWithVersions,
  CustomBotCurrentView,
} from "@/types/custom-bots";

interface MobileCustomBotDetailProps {
  bot: CustomBotWithVersions;
  currentBotView: CustomBotCurrentView;
  selectedVersion: string;
  onVersionChange: (version: string) => void;
}

export function MobileCustomBotDetail({
  bot,
  currentBotView,
  selectedVersion,
  onVersionChange,
}: MobileCustomBotDetailProps) {
  const router = useRouter();

  return (
    <div className="flex flex-col h-[calc(100vh-3rem)]">
      {/* Header with back arrow */}
      <div className="border-b p-3 flex items-center gap-3 flex-shrink-0">
        <Button
          variant="ghost"
          size="icon"
          className="h-8 w-8"
          onClick={() => router.push("/custom-bots")}
        >
          <ArrowLeft className="h-4 w-4" />
        </Button>
        <div className="min-w-0 flex-1">
          <h1 className="text-sm font-medium truncate">{bot.name}</h1>
          <p className="text-xs text-muted-foreground">v{selectedVersion}</p>
        </div>
        <Badge variant="outline" className="text-xs flex-shrink-0">
          {currentBotView.config?.type || "Bot"}
        </Badge>
      </div>

      {/* Tabbed content */}
      <Tabs defaultValue="schema" className="flex-1 flex flex-col min-h-0">
        <TabsList className="w-full rounded-none border-b bg-transparent h-10 flex-shrink-0">
          <TabsTrigger value="schema" className="flex-1 text-xs">
            Schema
          </TabsTrigger>
          <TabsTrigger value="readme" className="flex-1 text-xs">
            Readme
          </TabsTrigger>
          <TabsTrigger value="info" className="flex-1 text-xs">
            Info
          </TabsTrigger>
        </TabsList>

        <TabsContent value="schema" className="flex-1 m-0 overflow-auto p-4">
          <SchemaDisplay
            schema={currentBotView.config?.schema?.bot}
            botName={bot.name}
          />
        </TabsContent>

        <TabsContent value="readme" className="flex-1 m-0 overflow-auto p-4">
          {currentBotView.config?.readme ? (
            <ReadmeComponent readme={currentBotView.config.readme} />
          ) : (
            <div className="flex flex-col items-center justify-center py-12 text-muted-foreground">
              <BookOpen className="h-8 w-8 mb-2" />
              <p className="text-sm">No README available</p>
            </div>
          )}
        </TabsContent>

        <TabsContent value="info" className="flex-1 m-0 overflow-auto p-4">
          <div className="space-y-4">
            <StatusHeader bot={currentBotView} />
            <BotInfo
              bot={currentBotView}
              selectedVersion={selectedVersion}
              onVersionChange={onVersionChange}
            />
            <VersionHistory
              versions={bot.versions}
              selectedVersion={selectedVersion}
              onVersionChange={onVersionChange}
            />
            <ActionButtons
              bot={currentBotView}
              onBack={() => router.push("/custom-bots")}
              selectedVersion={selectedVersion}
            />
          </div>
        </TabsContent>
      </Tabs>
    </div>
  );
}
