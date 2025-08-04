import React, { useState } from 'react';
import { Button } from '@/components/ui/button';
import {
  ArrowLeft,
  Copy,
  Terminal,
  FileText,
  Play,
  BarChart3,
} from 'lucide-react';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { CustomBotWithVersions } from '@/types/custom-bots';
import { canBotBeBacktested } from '@/lib/utils';
import { useToast } from '@/hooks/use-toast';
import { useRouter } from 'next/navigation';
import { useAuth } from '@/contexts/auth-context';
import semver from 'semver';


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
  const router = useRouter();
  const { user } = useAuth();

  const [isDeployModalOpen, setIsDeployModalOpen] = useState(false);

  const versionData = bot.versions.find((v) => v.version === selectedVersion);
  const isVersionApproved = true; // Simplified - remove marketplace approval logic
  const isDeclinedBot = false; // Simplified


  // Check if this is the latest version
  const isLatestVersion = selectedVersion === bot.latestVersion;

  // Generate CLI deployment command
  const deployCommand = isVersionApproved
    ? `the0 deploy ${bot.name} config.json`
    : `the0 custom-bot deploy`;

  // Check if bot has README content
  const hasReadme = versionData?.config?.readme || bot.versions?.[0]?.config?.readme;


  const handleCopyCommand = () => {
    navigator.clipboard.writeText(deployCommand);
    toast({
      title: 'Command Copied',
      description: 'CLI deployment command copied to clipboard.',
    });
  };



  const handleDeploy = () => {
    router.push(
      `/deploy/${encodeURIComponent(bot.name)}/${encodeURIComponent(selectedVersion)}`,
    );
  };

  const handleBacktest = () => {
    router.push(
      `/backtests/create?name=${encodeURIComponent(bot.name)}&version=${encodeURIComponent(selectedVersion)}`,
    );
  };

  return (
    <>
      <div className="flex flex-col sm:flex-row gap-3">
        <Button variant="outline" onClick={onBack} className="gap-2">
          <ArrowLeft className="h-4 w-4" />
          Back to Custom Bots
        </Button>

        <Button onClick={handleDeploy} className="gap-2">
          <Play className="h-4 w-4" />
          Deploy Bot
        </Button>
        
        {versionData?.config && canBotBeBacktested(versionData.config) && (
          <Button
            variant="outline"
            onClick={handleBacktest}
            className="gap-2"
          >
            <BarChart3 className="h-4 w-4" />
            Backtest
          </Button>
        )}
        
        <Button
          variant="outline"
          onClick={() => setIsDeployModalOpen(true)}
          className="gap-2"
        >
          <Terminal className="h-4 w-4" />
          CLI Instructions
        </Button>
      </div>

      {/* CLI Deployment Modal */}
      <Dialog open={isDeployModalOpen} onOpenChange={setIsDeployModalOpen}>
        <DialogContent className="max-w-2xl">
          <DialogHeader>
            <DialogTitle className="flex items-center gap-2">
              <Terminal className="h-5 w-5" />
              Deploy {bot.name} v{selectedVersion}
            </DialogTitle>
            <DialogDescription>
              Use the the0 CLI to deploy your bot as a running instance.
            </DialogDescription>
          </DialogHeader>

          <div className="space-y-4">
            {/* CLI Command */}
            <div className="space-y-2">
              <Label className="text-sm font-medium">
                CLI Deployment Command
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
                  className="gap-2"
                >
                  <Copy className="h-4 w-4" />
                  Copy
                </Button>
              </div>
              <p className="text-xs text-muted-foreground">
                Run this command in your terminal where the the0 CLI is installed.
              </p>
            </div>

            {/* Configuration Instructions */}
            <div className="space-y-2">
              <Label className="text-sm font-medium flex items-center gap-2">
                <FileText className="h-4 w-4" />
                2. Configuration Setup
              </Label>
              <div className="p-4 bg-blue-50 dark:bg-blue-950/20 rounded-lg border border-blue-200 dark:border-blue-800">
                <div className="space-y-3">
                  {hasReadme ? (
                    <div className="space-y-2">
                      <p className="text-sm text-blue-800 dark:text-blue-200 font-medium">
                        📖 Configuration details are available in this
                        bot&#39;s README
                      </p>
                      <p className="text-xs text-blue-700 dark:text-blue-300">
                        The bot author has provided specific configuration
                        instructions and examples. Check the README section
                        below for detailed setup information.
                      </p>
                    </div>
                  ) : (
                    <div className="space-y-2">
                      <p className="text-sm text-blue-800 dark:text-blue-200 font-medium">
                        ⚙️ Configuration Required
                      </p>
                      <p className="text-xs text-blue-700 dark:text-blue-300">
                        This bot requires configuration before deployment. The
                        CLI will prompt you for the necessary settings when
                        you run the deploy command.
                      </p>
                    </div>
                  )}

                  <div className="text-xs text-blue-600 dark:text-blue-400 space-y-1">
                    <p>
                      <strong>Bot Type:</strong>{' '}
                      {versionData?.config?.type || 'Unknown'}
                    </p>
                    {versionData?.config?.type === 'scheduled' && (
                      <p>
                        <strong>Scheduling:</strong> This bot runs on a
                        schedule you configure
                      </p>
                    )}
                    {versionData?.config?.type === 'realtime' && (
                      <p>
                        <strong>Real-time:</strong> This bot monitors markets
                        continuously
                      </p>
                    )}
                    {versionData?.config?.type === 'event' && (
                      <p>
                        <strong>Event-driven:</strong> This bot responds to
                        specific market events
                      </p>
                    )}
                  </div>
                </div>
              </div>
            </div>

            {/* Instructions */}
            <div className="p-4 rounded-lg bg-gray-50 dark:bg-gray-950/20">
              <h4 className="text-sm font-medium mb-2">
                Next Steps:
              </h4>
              <ol className="text-sm text-muted-foreground space-y-1 list-decimal list-inside">
                <li>Install the the0 CLI if you haven&#39;t already</li>
                <li>
                  {hasReadme
                    ? "Review the bot's README for configuration details"
                    : 'Prepare your bot configuration settings'}
                </li>
                <li>Run the deployment command in your terminal</li>
                <li>Follow the CLI prompts to configure your bot</li>
                <li>
                  Monitor your bot&#39;s performance in the the0 dashboard
                </li>
              </ol>
            </div>
          </div>
        </DialogContent>
      </Dialog>
    </>
  );
};
