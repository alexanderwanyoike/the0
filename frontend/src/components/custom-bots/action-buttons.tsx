import React, { useState } from "react";
import { Button } from "@/components/ui/button";
import {
  ArrowLeft,
  Copy,
  Terminal,
  FileText,
  ExternalLink,
} from "lucide-react";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { CustomBotWithVersions } from "@/types/custom-bots";
import { useToast } from "@/hooks/use-toast";

interface ActionButtonsProps {
  bot: CustomBotWithVersions;
  selectedVersion: string;
  onBack: () => void;
}

export const ActionButtons: React.FC<ActionButtonsProps> = ({
  bot,
  selectedVersion,
  onBack,
}) => {
  const { toast } = useToast();
  const [isDeployModalOpen, setIsDeployModalOpen] = useState(false);

  const versionData = bot.versions.find((v) => v.version === selectedVersion);

  // CLI deployment command
  const deployCommand = `the0 bot deploy config.json`;

  // Check if bot has README content
  const hasReadme =
    versionData?.config?.readme || bot.versions?.[0]?.config?.readme;

  const handleCopyCommand = () => {
    navigator.clipboard.writeText(deployCommand);
    toast({
      title: "Command Copied",
      description: "CLI deployment command copied to clipboard.",
    });
  };

  // Example config.json content
  const exampleConfig = JSON.stringify(
    {
      name: "my-bot-instance",
      type: `${versionData?.config?.type || "scheduled"}/${bot.name}`,
      version: selectedVersion,
      ...(versionData?.config?.type === "scheduled" && {
        schedule: "0 9 * * *",
      }),
      // Add placeholder for other fields
    },
    null,
    2,
  );

  const handleCopyConfig = () => {
    navigator.clipboard.writeText(exampleConfig);
    toast({
      title: "Config Template Copied",
      description: "Example config.json copied to clipboard.",
    });
  };

  return (
    <>
      <div className="flex flex-col sm:flex-row gap-3">
        <Button variant="outline" onClick={onBack} className="gap-2">
          <ArrowLeft className="h-4 w-4" />
          Back to Custom Bots
        </Button>

        <Button onClick={() => setIsDeployModalOpen(true)} className="gap-2">
          <Terminal className="h-4 w-4" />
          Deploy via CLI
        </Button>
      </div>

      {/* CLI Deployment Modal */}
      <Dialog open={isDeployModalOpen} onOpenChange={setIsDeployModalOpen}>
        <DialogContent className="max-w-2xl max-h-[90vh] overflow-y-auto">
          <DialogHeader>
            <DialogTitle className="flex items-center gap-2">
              <Terminal className="h-5 w-5" />
              Deploy {bot.name} v{selectedVersion}
            </DialogTitle>
            <DialogDescription>
              Use the the0 CLI to deploy your bot as a running instance.
            </DialogDescription>
          </DialogHeader>

          <div className="space-y-6">
            {/* Step 1: CLI Command */}
            <div className="space-y-2">
              <Label className="text-sm font-medium">
                1. Run the deployment command
              </Label>
              <div className="flex gap-2">
                <Input
                  value={deployCommand}
                  readOnly
                  className="font-mono text-sm bg-muted"
                />
                <Button
                  variant="outline"
                  size="sm"
                  onClick={handleCopyCommand}
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

            {/* Step 2: Config File */}
            <div className="space-y-2">
              <Label className="text-sm font-medium flex items-center gap-2">
                <FileText className="h-4 w-4" />
                2. Create your config.json
              </Label>
              <div className="relative">
                <pre className="text-xs bg-muted p-4 rounded-lg font-mono overflow-auto max-h-[200px]">
                  {exampleConfig}
                </pre>
                <Button
                  variant="outline"
                  size="sm"
                  onClick={handleCopyConfig}
                  className="absolute top-2 right-2 gap-2"
                >
                  <Copy className="h-4 w-4" />
                  Copy
                </Button>
              </div>
              <p className="text-xs text-muted-foreground">
                Fill in the configuration values based on the bot schema. See
                the Schema section on this page for all available parameters.
              </p>
            </div>

            {/* Bot Info */}
            <div className="p-4 bg-blue-50 dark:bg-blue-950/20 rounded-lg border border-blue-200 dark:border-blue-800">
              <div className="space-y-3">
                {hasReadme && (
                  <div className="space-y-1">
                    <p className="text-sm text-blue-800 dark:text-blue-200 font-medium">
                      Configuration details available in README
                    </p>
                    <p className="text-xs text-blue-700 dark:text-blue-300">
                      Check the README section below for detailed setup
                      information from the bot author.
                    </p>
                  </div>
                )}

                <div className="text-xs text-blue-600 dark:text-blue-400 space-y-1">
                  <p>
                    <strong>Bot Type:</strong>{" "}
                    {versionData?.config?.type || "Unknown"}
                  </p>
                  <p>
                    <strong>Runtime:</strong>{" "}
                    {versionData?.config?.runtime || "Unknown"}
                  </p>
                  {versionData?.config?.type === "scheduled" && (
                    <p>
                      <strong>Note:</strong> Scheduled bots require a cron
                      expression in the schedule field
                    </p>
                  )}
                </div>
              </div>
            </div>

            {/* Helpful CLI Commands */}
            <div className="space-y-2">
              <Label className="text-sm font-medium">
                Helpful CLI Commands
              </Label>
              <div className="space-y-2 text-xs font-mono bg-muted p-4 rounded-lg">
                <p className="text-muted-foreground"># Get bot schema</p>
                <p>
                  the0 custom-bot schema bot {selectedVersion} {bot.name}
                </p>
                <p className="text-muted-foreground mt-2">
                  # List your deployed bots
                </p>
                <p>the0 bot list</p>
                <p className="text-muted-foreground mt-2"># View bot logs</p>
                <p>the0 bot logs &lt;bot_id&gt;</p>
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
    </>
  );
};
