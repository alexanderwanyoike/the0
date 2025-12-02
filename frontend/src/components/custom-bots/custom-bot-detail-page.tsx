import { useRouter } from "next/navigation";
import { useState } from "react";
import { useCustomBot } from "@/hooks/custom-bots/use-custom-bot";
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert";
import { AlertTriangle, ArrowLeft, Bot } from "lucide-react";
import { Button } from "@/components/ui/button";
import React from "react";
import { StatusHeader } from "@/components/custom-bots/status-header";
import { ScanProgress } from "@/components/custom-bots/scan-progress";
import { SecurityReport } from "@/components/custom-bots/security-report";
import { BotInfo } from "@/components/custom-bots/bot-info";
import { VersionHistory } from "@/components/custom-bots/version-history";
import { ActionButtons } from "@/components/custom-bots/action-buttons";
import { ReadmeComponent } from "@/components/custom-bots/readme-component";
import { LoadingState } from "@/components/custom-bots/loading-detail-state";
import { AIReviewSection } from "@/components/custom-bots/ai-review-section";
import { SchemaDisplay } from "@/components/custom-bots/schema-display";

export const CustomBotDetailPage = ({ botName }: any) => {
  const router = useRouter();
  const { bot, loading, error } = useCustomBot(botName);

  // Track selected version - default to latest
  const [selectedVersion, setSelectedVersion] = useState<string>("");

  // Set initial selected version when bot loads
  React.useEffect(() => {
    if (bot && !selectedVersion) {
      setSelectedVersion(bot.latestVersion);
    }
  }, [bot, selectedVersion]);

  const handleBack = () => {
    router.push("/custom-bots");
  };

  const handleVersionChange = (version: string) => {
    setSelectedVersion(version);
  };

  if (loading) {
    return (
      <div className="container max-w-7xl mx-auto py-6 px-4 lg:px-6">
        <LoadingState />
      </div>
    );
  }

  if (error || !bot) {
    return (
      <div className="container max-w-7xl mx-auto py-6 px-4 lg:px-6">
        <Alert variant="destructive">
          <AlertTriangle className="h-4 w-4" />
          <AlertTitle>Bot Not Found</AlertTitle>
          <AlertDescription>
            The custom bot &#34;{botName}&#34; was not found or you don&#39;t
            have permission to view it.
          </AlertDescription>
        </Alert>
        <Button variant="outline" onClick={handleBack} className="mt-4 gap-2">
          <ArrowLeft className="h-4 w-4" />
          Back to Custom Bots
        </Button>
      </div>
    );
  }

  // Get the data for the selected version
  const selectedVersionData =
    bot.versions.find((v) => v.version === selectedVersion) || bot.versions[0];

  // Create a combined bot object with selected version data
  const currentBotView = {
    ...bot,
    version: selectedVersionData.version,
    status: selectedVersionData.status,
    config: selectedVersionData.config,
    filePath: selectedVersionData.filePath,
    createdAt: selectedVersionData.createdAt,
    // Get review data from the version
    review: selectedVersionData.review,
  };

  return (
    <div className="container max-w-7xl mx-auto py-6 px-4 lg:px-6">
      {/* Header */}
      <div className="mb-8">
        <div className="flex items-center gap-3 mb-4">
          <div className="flex items-center justify-center h-12 w-12 rounded-lg bg-primary/10">
            <Bot className="h-6 w-6 text-primary" />
          </div>
          <div>
            <h1 className="text-3xl font-bold">{bot.name}</h1>
            <p className="text-muted-foreground">
              Custom Bot Security Review - Version {selectedVersion}
            </p>
          </div>
        </div>

        <StatusHeader bot={currentBotView} />
      </div>
      {/* Action Buttons */}
      <div className="mt-8 pb-6">
        <ActionButtons
          bot={currentBotView}
          onBack={handleBack}
          selectedVersion={selectedVersion}
        />
      </div>

      {/* Main Content */}
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Left Column - Main Content */}
        <div className="lg:col-span-2 space-y-6">
          {/* AI Review Section */}
          <AIReviewSection
            review={currentBotView.review}
            status={currentBotView.status}
          />

          {/* Schema Display Section */}
          <SchemaDisplay
            schema={currentBotView.config?.schema?.bot}
            botName={bot.name}
          />

          {/* README Section - Show if available in review or config */}
          {(currentBotView.review?.readme || currentBotView.config?.readme) && (
            <ReadmeComponent
              readme={
                currentBotView.review?.readme || currentBotView.config?.readme
              }
            />
          )}
        </div>

        {/* Right Column - Sidebar */}
        <div className="space-y-6">
          <BotInfo
            bot={currentBotView}
            selectedVersion={selectedVersion}
            onVersionChange={handleVersionChange}
          />
          <VersionHistory
            versions={bot.versions}
            selectedVersion={selectedVersion}
            onVersionChange={handleVersionChange}
          />
          <ScanProgress
            review={currentBotView.review}
            status={currentBotView.status}
          />
          <SecurityReport
            review={currentBotView.review}
            status={currentBotView.status}
          />
        </div>
      </div>
    </div>
  );
};
