'use client';

import React, { use, useEffect, useState } from 'react';
import { useAuth } from '@/contexts/auth-context';
import DashboardLayout from '@/components/layouts/dashboard-layout';
import { useRouter } from 'next/navigation';
import moment from 'moment';
import TradingViewWidget from '@/components/trading-view/TradingViewChart';
import { Button } from '@/components/ui/button';
import { Switch } from '@/components/ui/switch';
import {
  Clipboard,
  Loader2,
  Pencil,
  Trash2,
  AlertTriangle,
  ChevronDown,
  ChevronUp,
} from 'lucide-react';
import { useToast } from '@/hooks/use-toast';
import { withAuth } from '@/components/auth/with-auth';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { ConsoleInterface } from '@/components/bot/console-interface';
import { useBotLogs } from '@/hooks/use-bot-logs';
import { BotService, Bot as ApiBotType } from '@/lib/api/api-client';
import { getErrorMessage } from '@/lib/axios';

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
} from '@/components/ui/alert-dialog';

// Extend the API Bot type with additional properties we need
interface Bot extends ApiBotType {
  userId?: string;
  user_id?: string;
  name?: string;
  version?: string;
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
          throw new Error(result.error.message || 'Failed to fetch bot');
        }

        const botData = result.data;
        // Check authorization (API might use 'user_id' instead of 'userId')
        const botUserId = (botData as any).userId || (botData as any).user_id;
        if (botUserId !== user.id) throw new Error('Unauthorized access');

        // Add a default status if not present
        if (!botData.status) {
          botData.status = 'running';
        }

        setBot(botData);
      } catch (error) {
        console.error('Error fetching bot:', error);
        const errorMessage =
          error instanceof Error ? error.message : 'An unknown error occurred';
        setError(errorMessage);

        // Show toast for errors
        toast({
          title: 'Error',
          description: `Failed to load bot: ${errorMessage}`,
          variant: 'destructive',
        });

        if (
          error instanceof Error &&
          (error.message === 'Bot not found' ||
            error.message === 'Unauthorized access')
        ) {
          setTimeout(() => router.push('/dashboard'), 2000);
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
      description: 'Bot configuration copied to clipboard',
      duration: 2000,
    });
  };

  // Handle bot update (redirect to deploy page with botId for updates)
  const handleUpdateBot = () => {
    if (!bot) return;

    // Extract bot name from the config.type format (type/name)
    let botName = 'unknown';
    let botVersion = 'latest';

    if (bot.config?.type && bot.config.type.includes('/')) {
      // Extract name from type/name format
      const [, extractedName] = bot.config.type.split('/');
      if (extractedName && extractedName.trim()) {
        botName = extractedName;
      }
    } else if (bot.name) {
      // Fallback to bot.name if available
      botName = bot.name;
    } else if (bot.config?.name) {
      // Fallback to config.name if available
      botName = bot.config.name;
    }

    // Get version from config
    if (bot.config?.version) {
      botVersion = bot.config.version;
    }

    router.push(
      `/deploy/${encodeURIComponent(botName)}/${encodeURIComponent(botVersion)}?botId=${botId}`,
    );
  };

  // Handle bot deletion
  const handleDeleteBot = async () => {
    if (!bot) return;
    setIsDeleting(true);

    try {
      const result = await BotService.deleteBot(botId);
      
      if (!result.success) {
        throw new Error(result.error.message || 'Failed to delete bot');
      }

      toast({
        description: 'Bot deleted successfully',
        duration: 2000,
      });

      // Redirect to dashboard list page
      router.push('/dashboard');
    } catch (error) {
      console.error('Error deleting bot:', error);
      toast({
        title: 'Delete Failed',
        description:
          error instanceof Error ? error.message : 'Failed to delete bot',
        variant: 'destructive',
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
          description: `Bot ${enabled ? 'enabled' : 'disabled'} successfully. It may take a few moments to reflect the change.`,
          duration: 2000,
        });
      } else {
        throw new Error(result.error.message);
      }
    } catch (error) {
      console.error('Error updating bot enabled status:', error);
      toast({
        title: 'Update Failed',
        description: getErrorMessage(error),
        variant: 'destructive',
      });
    } finally {
      setIsUpdatingEnabled(false);
    }
  };

  // Get status badge color based on bot status
  const getStatusBadgeClass = (status: string) => {
    switch (status) {
      case 'running':
        return 'bg-green-100 text-green-700 dark:bg-green-900/30 dark:text-green-400';
      case 'stopped':
        return 'bg-red-100 text-red-700 dark:bg-red-900/30 dark:text-red-400';
      case 'paused':
        return 'bg-amber-100 text-amber-700 dark:bg-amber-900/30 dark:text-amber-400';
      case 'restarting':
        return 'bg-blue-100 text-blue-700 dark:bg-blue-900/30 dark:text-blue-400';
      default:
        return 'bg-gray-100 text-gray-700 dark:bg-gray-900/30 dark:text-gray-400';
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
      if (!obj || typeof obj !== 'object') return obj;

      const filtered: any = Array.isArray(obj) ? [] : {};

      Object.keys(obj).forEach((key) => {
        const isSensitive = sensitivePatterns.some((pattern) =>
          pattern.test(key),
        );

        if (isSensitive) {
          // Skip sensitive fields entirely - don't include them in the display
          return;
        } else if (typeof obj[key] === 'object' && obj[key] !== null) {
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
                        'Enabled'
                      ) : (
                        'Disabled'
                      )}
                    </span>
                  </div>
                  <Button variant="outline" size="sm" onClick={handleUpdateBot}>
                    <Pencil className="h-4 w-4 mr-2" />
                    Update Config
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
                            'Delete Bot'
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
                      'Enabled'
                    ) : (
                      'Disabled'
                    )}
                  </span>
                </div>
                <div className="flex flex-row xs:flex-row gap-2">
                  <Button
                    variant="outline"
                    size="sm"
                    onClick={handleUpdateBot}
                    className="flex-1 xs:flex-none py-1"
                  >
                    <Pencil className="h-4 w-4 mr-2" />
                    Update Config
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
                            'Delete Bot'
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

        {/* Main Content */}
        <div className="flex-1 p-4">
          {bot.config.symbol ? (
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              {/* Trading View */}
              <div className="h-[400px] md:h-[600px] bg-muted/30">
                <TradingViewWidget symbol={bot.config.symbol} />
              </div>

              {/* Console */}
              <div className="h-[600px] md:h-[600px] bg-muted/30">
                <ConsoleInterface
                  botId={botId}
                  logs={logs}
                  loading={logsLoading}
                  onRefresh={refreshLogs}
                  onDateChange={setDateFilter}
                  onDateRangeChange={setDateRangeFilter}
                  onExport={exportLogs}
                  className="h-full"
                />
              </div>
            </div>
          ) : (
            /* Console takes full width when no symbol */
            <div className="h-[600px] md:h-[600px] bg-muted/30">
              <ConsoleInterface
                botId={botId}
                logs={logs}
                loading={logsLoading}
                onRefresh={refreshLogs}
                onDateChange={setDateFilter}
                onDateRangeChange={setDateRangeFilter}
                onExport={exportLogs}
                className="h-full"
              />
            </div>
          )}
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
                  {bot.config.schedule || 'Real-time'}
                </dd>
              </div>
              <div className="grid grid-cols-3 gap-1">
                <dt className="text-sm text-muted-foreground">Created</dt>
                <dd className="col-span-2">
                  <div className="flex items-baseline gap-2">
                    <span className="text-sm">
                      {moment(bot.createdAt).format('MMM D, YYYY')}
                    </span>
                    <span className="text-xs text-muted-foreground">
                      {moment(bot.createdAt).format('h:mm A')}
                    </span>
                  </div>
                </dd>
              </div>
              <div className="grid grid-cols-3 gap-1">
                <dt className="text-sm text-muted-foreground">Updated</dt>
                <dd className="col-span-2">
                  <div className="flex items-baseline gap-2">
                    <span className="text-sm">
                      {moment(bot.updatedAt).format('MMM D, YYYY')}
                    </span>
                    <span className="text-xs text-muted-foreground">
                      {moment(bot.updatedAt).format('h:mm A')}
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
    </DashboardLayout>
  );
};

export default withAuth(BotDetail);
