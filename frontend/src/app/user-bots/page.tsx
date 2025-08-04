'use client';

import React, { useState } from 'react';
import Link from 'next/link';
import { withAuth } from '@/components/auth/with-auth';
import DashboardLayout from '@/components/layouts/dashboard-layout';
import { useUserBots } from '@/hooks/use-user-bots';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { Separator } from '@/components/ui/separator';
import {
  Bot,
  Calendar,
  ExternalLink,
  Terminal,
  Info,
  Play,
  ChevronDown,
  BarChart3,
} from 'lucide-react';
import { Skeleton } from '@/components/ui/skeleton';
import { canBotBeBacktested } from '@/lib/utils';

function UserBotsPage() {
  const { userBots, loading, error, refetch } = useUserBots();
  const [selectedVersions, setSelectedVersions] = useState<
    Record<string, string>
  >({});

  const handleVersionChange = (botId: string, version: string) => {
    setSelectedVersions((prev) => ({ ...prev, [botId]: version }));
  };

  const getSelectedVersion = (bot: any) => {
    if (!bot.customBot?.versions?.length) return null;
    return (
      selectedVersions[bot.id] ||
      bot.customBot.latestVersion ||
      bot.customBot.versions[0]?.version
    );
  };


  const getApprovedVersions = (bot: any) => {
    if (!bot.customBot?.versions) return [];
    return bot.customBot.versions.filter(
      (v: any) => v.status === 'approved' || v.status === 'published',
    );
  };

  const getSelectedVersionConfig = (bot: any) => {
    const selectedVersion = getSelectedVersion(bot);
    if (!selectedVersion || !bot.customBot?.versions) return null;
    const versionData = bot.customBot.versions.find(
      (v: any) => v.version === selectedVersion,
    );
    return versionData?.config || null;
  };

  if (loading) {
    return (
      <DashboardLayout>
        <div className="container max-w-6xl py-4 px-4 sm:py-8 sm:px-6">
          <div className="mb-6 sm:mb-8">
            <h1 className="text-xl sm:text-2xl font-medium">My Bots</h1>
            <p className="text-sm text-muted-foreground mt-1">
              Manage your installed bots and deploy them to your infrastructure
            </p>
          </div>

          <div className="space-y-4">
            {Array.from({ length: 3 }).map((_, i) => (
              <Card key={i}>
                <CardContent className="p-6">
                  <div className="flex items-center justify-between">
                    <div className="space-y-2">
                      <Skeleton className="h-5 w-32" />
                      <Skeleton className="h-4 w-24" />
                    </div>
                    <Skeleton className="h-9 w-24" />
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>
        </div>
      </DashboardLayout>
    );
  }

  if (error) {
    return (
      <DashboardLayout>
        <div className="container max-w-6xl py-4 px-4 sm:py-8 sm:px-6">
          <div className="mb-6 sm:mb-8">
            <h1 className="text-xl sm:text-2xl font-medium">My Bots</h1>
            <p className="text-sm text-muted-foreground mt-1">
              Manage your installed bots and deploy them to your infrastructure
            </p>
          </div>

          <Alert variant="destructive">
            <AlertDescription>
              Failed to load your bots: {error}
              <Button
                variant="outline"
                size="sm"
                onClick={refetch}
                className="ml-2"
              >
                Retry
              </Button>
            </AlertDescription>
          </Alert>
        </div>
      </DashboardLayout>
    );
  }

  const formatDate = (date: string | Date) => {
    let dateObj: Date;

    if (date instanceof Date) {
      dateObj = date;
    } else if (typeof date === 'string') {
      dateObj = new Date(date);
    } else {
      return 'Unknown';
    }

    // Validate the date
    if (isNaN(dateObj.getTime())) {
      return 'Invalid Date';
    }

    return new Intl.DateTimeFormat('en-US', {
      year: 'numeric',
      month: 'long',
      day: 'numeric',
    }).format(dateObj);
  };

  return (
    <DashboardLayout>
      <div className="container max-w-6xl py-4 px-4 sm:py-8 sm:px-6">
        {/* Header */}
        <div className="mb-6 sm:mb-8">
          <h1 className="text-xl sm:text-2xl font-medium">My Bots</h1>
          <p className="text-sm text-muted-foreground mt-1">
            Manage your installed bots and deploy them to your infrastructure
          </p>
        </div>

        {/* Deployment Instructions Alert */}
        <Alert className="mb-6 sm:mb-8">
          <Terminal className="h-4 w-4" />
          <AlertDescription>
            <div className="space-y-2">
              <p className="font-medium">Deploy your bots:</p>
              <p className="text-sm">
                Select a version from the dropdown and click Deploy to configure
                and launch your bot instances.
              </p>
            </div>
          </AlertDescription>
        </Alert>

        {/* Bots List */}
        {userBots.length === 0 ? (
          <Card>
            <CardContent className="py-12 text-center">
              <Bot className="mx-auto h-12 w-12 text-muted-foreground mb-4" />
              <h3 className="text-lg font-medium mb-2">No bots installed</h3>
              <p className="text-muted-foreground mb-6">
                You haven&#39;t installed any bots yet. Browse the marketplace
                to find and install bots.
              </p>
              <Link href="/marketplace/search">
                <Button>
                  Browse Marketplace
                  <ExternalLink className="ml-2 h-4 w-4" />
                </Button>
              </Link>
            </CardContent>
          </Card>
        ) : (
          <div className="space-y-4">
            {userBots.map((bot) => (
              <Card key={bot.id} className="hover:shadow-md transition-shadow">
                <CardHeader className="pb-3">
                  <div className="flex items-start justify-between">
                    <div className="flex items-center gap-3">
                      <div className="flex items-center justify-center h-10 w-10 rounded-lg bg-primary/10">
                        <Bot className="h-5 w-5 text-primary" />
                      </div>
                      <div>
                        <CardTitle className="text-lg font-medium">
                          {bot.customBotName}
                        </CardTitle>
                        <div className="flex items-center gap-2 mt-1 text-sm text-muted-foreground">
                          <Calendar className="h-3 w-3" />
                          <span>Installed {formatDate(bot.acquiredAt)}</span>
                          {bot.customBot && (
                            <span>
                              • {getApprovedVersions(bot).length} version
                              {getApprovedVersions(bot).length !== 1
                                ? 's'
                                : ''}{' '}
                              available
                            </span>
                          )}
                        </div>
                      </div>
                    </div>
                    <Badge
                      variant="secondary"
                      className="bg-green-50 text-green-700 border-green-200"
                    >
                      ✓ Installed
                    </Badge>
                  </div>
                </CardHeader>

                <Separator className="mx-6" />

                <CardContent className="pt-4">
                  {bot.customBot && getApprovedVersions(bot).length > 0 ? (
                    <div className="space-y-4">
                      {/* Version Selection */}
                      <div className="flex items-center gap-3">
                        <label className="text-sm font-medium min-w-0">
                          Version:
                        </label>
                        <Select
                          value={getSelectedVersion(bot) || ''}
                          onValueChange={(version) =>
                            handleVersionChange(bot.id, version)
                          }
                        >
                          <SelectTrigger className="w-[200px]">
                            <SelectValue placeholder="Select version" />
                          </SelectTrigger>
                          <SelectContent>
                            {getApprovedVersions(bot).map((version: any) => (
                              <SelectItem
                                key={version.version}
                                value={version.version}
                              >
                                v{version.version}
                                {version.version ===
                                  bot?.customBot?.latestVersion && (
                                  <span className="ml-2 text-xs text-primary">
                                    (latest)
                                  </span>
                                )}
                              </SelectItem>
                            ))}
                          </SelectContent>
                        </Select>
                      </div>

                      {/* Action Buttons */}
                      <div className="flex items-center justify-between">
                        <div className="text-sm text-muted-foreground">
                          <p>Configure and deploy your bot instance</p>
                        </div>
                        <div className="flex gap-2">
                          <Link
                            href={`/marketplace/bots/${encodeURIComponent(bot.customBotName)}`}
                          >
                            <Button variant="outline" size="sm">
                              View Details
                              <ExternalLink className="ml-2 h-3 w-3" />
                            </Button>
                          </Link>
                          {getSelectedVersion(bot) && (
                            <>
                              {getSelectedVersionConfig(bot) &&
                                canBotBeBacktested(
                                  getSelectedVersionConfig(bot),
                                ) && (
                                  <Link
                                    href={`/backtests/create?name=${encodeURIComponent(bot.customBotName)}&version=${encodeURIComponent(getSelectedVersion(bot))}`}
                                  >
                                    <Button
                                      variant="outline"
                                      size="sm"
                                      className="gap-2"
                                    >
                                      <BarChart3 className="h-3 w-3" />
                                      Backtest
                                    </Button>
                                  </Link>
                                )}

                              <Link
                                href={`/deploy/${encodeURIComponent(bot.customBotName)}/${encodeURIComponent(getSelectedVersion(bot))}`}
                              >
                                <Button size="sm" className="gap-2">
                                  <Play className="h-3 w-3" />
                                  Deploy
                                </Button>
                              </Link>
                            </>
                          )}
                        </div>
                      </div>
                    </div>
                  ) : (
                    <div className="flex items-center justify-between">
                      <div className="text-sm text-muted-foreground">
                        <p>No approved versions available for deployment</p>
                      </div>
                      <div className="flex gap-2">
                        <Link
                          href={`/marketplace/bots/${encodeURIComponent(bot.customBotName)}`}
                        >
                          <Button variant="outline" size="sm">
                            View Details
                            <ExternalLink className="ml-2 h-3 w-3" />
                          </Button>
                        </Link>
                      </div>
                    </div>
                  )}
                </CardContent>
              </Card>
            ))}
          </div>
        )}

        {/* Additional Info */}
        {userBots.length > 0 && (
          <Alert className="mt-8">
            <Info className="h-4 w-4" />
            <AlertDescription>
              <p className="font-medium mb-1">Need help with deployment?</p>
              <p className="text-sm">
                Check out our documentation for detailed instructions on how to
                deploy and manage your bots.
              </p>
            </AlertDescription>
          </Alert>
        )}
      </div>
    </DashboardLayout>
  );
}

export default withAuth(UserBotsPage);
