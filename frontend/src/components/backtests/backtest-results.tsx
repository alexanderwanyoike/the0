'use client';

import React from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { Separator } from '@/components/ui/separator';
import { MetricsSummary } from './metrics-summary';
import { PlotsDisplay } from './plots-display';
import { TablesDisplay } from './tables-display';
import { Backtest } from '@/types/backtest';
import { useBacktest } from '@/hooks/backtests/use-backtest';
import {
  BarChart3,
  AlertCircle,
  Clock,
  CheckCircle,
  XCircle,
  Loader2,
  RefreshCcw,
  AlertTriangle,
} from 'lucide-react';

interface BacktestResultsProps {
  backtest: Backtest;
}

const ConfigurationSummary = ({ config }: { config: any }) => {
  const flattenConfig = (obj: any, prefix = ''): Array<[string, any]> => {
    const result: Array<[string, any]> = [];

    Object.entries(obj || {}).forEach(([key, value]) => {
      const fullKey = prefix ? `${prefix}/${key}` : key;

      if (value && typeof value === 'object' && !Array.isArray(value)) {
        result.push(...flattenConfig(value, fullKey));
      } else {
        result.push([fullKey, value]);
      }
    });

    return result;
  };

  const sensitiveKeys =
    /^(.*\.)?((api_?key|private_?key|public_?key|secret|password|token|client_?secret|client_?id|auth|credential|access_?token|refresh_?token)s?)$/i;

  return flattenConfig(config)
    .filter(
      ([key]) => !['type', 'version'].includes(key) && !sensitiveKeys.test(key),
    )
    .map(([key, value]) => (
      <div key={key}>
        <p className="text-sm font-medium text-muted-foreground capitalize">
          {key.replace(/([A-Z])/g, ' $1').trim()}
        </p>
        <p className="text-sm">
          {value == null ? (
            <span className="text-muted-foreground">null</span>
          ) : (
            String(value)
          )}
        </p>
      </div>
    ));
};

export function BacktestResults({ backtest }: BacktestResultsProps) {
  // Use the enhanced hook to get backtest data
  const { backtest: backtestData, loading, error, refetch } =
    useBacktest(backtest.id);

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'completed':
        return <CheckCircle className="h-4 w-4 text-green-600" />;
      case 'running':
        return <Loader2 className="h-4 w-4 text-blue-600 animate-spin" />;
      case 'pending':
        return <Clock className="h-4 w-4 text-yellow-600" />;
      case 'failed':
        return <XCircle className="h-4 w-4 text-red-600" />;
      default:
        return <AlertCircle className="h-4 w-4 text-gray-600" />;
    }
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'completed':
        return 'default';
      case 'running':
        return 'secondary';
      case 'pending':
        return 'outline';
      case 'failed':
        return 'destructive';
      default:
        return 'outline';
    }
  };

  const getErrorMessage = () => {
    const analysis = backtestData?.analysis || backtest.analysis;
    return analysis?.status === 'error' ? analysis.message : null;
  };

  const getStatusMessage = (status: string) => {
    switch (status) {
      case 'completed':
        return 'Backtest completed successfully. Results are ready for analysis.';
      case 'running':
        return 'Backtest is currently running. Please wait for completion.';
      case 'pending':
        return 'Backtest is queued for execution. It will start shortly.';
      case 'failed':
        return 'Backtest failed to complete. Please check the configuration and try again.';
      default:
        return 'Unknown backtest status.';
    }
  };

  // Show status for non-completed backtests
  if (backtest.status !== 'completed') {
    const errorMessage =
      backtest.status === 'failed' ? getErrorMessage() : null;

    return (
      <div className="space-y-6">
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <BarChart3 className="h-5 w-5" />
              Backtest Status
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="flex flex-col items-center justify-center py-12 space-y-4">
              <div className="flex items-center gap-3">
                {getStatusIcon(backtest.status)}
                <Badge
                  variant={getStatusColor(backtest.status)}
                  className="text-sm"
                >
                  {backtest.status.toUpperCase()}
                </Badge>
              </div>

              <p className="text-center text-muted-foreground max-w-md">
                {getStatusMessage(backtest.status)}
              </p>

              {errorMessage && (
                <div className="w-full max-w-2xl">
                  <p className="text-center text-muted-foreground mb-3">
                    Error details:
                  </p>
                  <code className="block p-4 bg-destructive/10 border border-destructive/20 rounded-md text-sm text-left whitespace-pre-wrap break-words">
                    {errorMessage}
                  </code>
                </div>
              )}

              {backtest.status === 'running' && (
                <p className="text-xs text-muted-foreground">
                  This page will automatically update when results are
                  available.
                </p>
              )}
            </div>
          </CardContent>
        </Card>

        {/* Configuration Summary for non-completed backtests */}
        <Card>
          <CardHeader>
            <CardTitle>Configuration</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
              <div>
                <p className="text-sm font-medium text-muted-foreground">
                  Bot Type
                </p>
                <p className="text-sm">{backtest.config.type}</p>
              </div>
              <div>
                <p className="text-sm font-medium text-muted-foreground">
                  Version
                </p>
                <p className="text-sm">v{backtest.config.version}</p>
              </div>
              {Object.entries(backtest.config)
                .filter(([key]) => !['type', 'version'].includes(key))
                .slice(0, 6)
                .map(([key, value]) => (
                  <div key={key}>
                    <p className="text-sm font-medium text-muted-foreground capitalize">
                      {key.replace(/([A-Z])/g, ' $1').trim()}
                    </p>
                    <p className="text-sm">{String(value)}</p>
                  </div>
                ))}
            </div>
          </CardContent>
        </Card>
      </div>
    );
  }

  // Show analysis loading state for completed backtests
  if (backtest.status === 'completed' && loading) {
    return (
      <Card>
        <CardContent className="py-8">
          <div className="flex flex-col items-center justify-center space-y-4">
            <Loader2 className="h-8 w-8 animate-spin text-primary" />
            <div className="text-center">
              <h3 className="font-medium mb-2">Loading Analysis Data</h3>
              <p className="text-sm text-muted-foreground">
                Fetching backtest analysis results...
              </p>
            </div>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Show analysis error state
  if (backtest.status === 'completed' && error) {
    return (
      <Card>
        <CardContent className="py-8">
          <div className="text-center space-y-4">
            <AlertTriangle className="h-12 w-12 mx-auto text-amber-500" />
            <div>
              <h3 className="font-medium mb-2">Failed to Load Analysis</h3>
              <p className="text-sm text-muted-foreground mb-4">
                {error}
              </p>
              <Button
                onClick={refetch}
                variant="outline"
                className="gap-2"
              >
                <RefreshCcw className="h-4 w-4" />
                Try Again
              </Button>
            </div>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Use analysis data from hook, fallback to backtest.analysis for backwards compatibility
  const analysis = backtestData?.analysis || backtest.analysis;

  // Check if analysis contains an error (failed backtest with completed status)
  if (analysis?.status === 'error') {
    return (
      <Card>
        <CardContent className="py-8">
          <div className="text-center space-y-4">
            <XCircle className="h-12 w-12 mx-auto text-red-600" />
            <div>
              <h3 className="font-medium mb-2">Backtest Failed</h3>
              <p className="text-sm text-muted-foreground mb-4">
                The backtest execution failed with the following error:
              </p>
              <code className="block p-4 bg-destructive/10 border border-destructive/20 rounded-md text-sm text-left whitespace-pre-wrap break-words max-w-2xl mx-auto">
                {analysis.message}
              </code>
            </div>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Show results for completed backtests
  if (!analysis) {
    return (
      <Card>
        <CardContent className="py-8">
          <div className="text-center space-y-4">
            <AlertCircle className="h-12 w-12 mx-auto mb-4 text-amber-500" />
            <div>
              <h3 className="font-medium mb-2">No Analysis Data Available</h3>
              <p className="text-sm text-muted-foreground mb-4">
                Backtest completed but no analysis data is available.
              </p>
              {backtest.status === 'completed' && (
                <Button
                  onClick={refetch}
                  variant="outline"
                  className="gap-2"
                >
                  <RefreshCcw className="h-4 w-4" />
                  Retry Analysis
                </Button>
              )}
            </div>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Support both legacy flat structure and new nested structure
  const { metrics, plots, tables } = analysis?.results || analysis || {};

  return (
    <div className="space-y-6">
      {/* Analysis Error Alert (when we have partial data) */}
      {backtest.status === 'completed' && error && analysis && (
        <Alert className="border-amber-200 bg-amber-50 dark:bg-amber-900/20">
          <AlertTriangle className="h-4 w-4 text-amber-600" />
          <AlertDescription className="text-amber-800 dark:text-amber-300">
            Some analysis data may be incomplete due to an error:{' '}
            {error}
          </AlertDescription>
        </Alert>
      )}

      <Separator />

      {/* Charts */}
      {plots && plots.length > 0 && (
        <div>
          <h2 className="text-lg font-semibold mb-4 flex items-center gap-2">
            <BarChart3 className="h-5 w-5" />
            Charts & Visualizations
          </h2>
          <PlotsDisplay plots={plots} />
        </div>
      )}

      {plots && plots.length > 0 && tables && tables.length > 0 && (
        <Separator />
      )}

      {/* Performance Metrics */}
      {metrics && Object.keys(metrics).length > 0 && (
        <div>
          <h2 className="text-lg font-semibold mb-4 flex items-center gap-2">
            <BarChart3 className="h-5 w-5" />
            Performance Metrics
          </h2>
          <MetricsSummary metrics={metrics} />
        </div>
      )}

      {metrics &&
        Object.keys(metrics).length > 0 &&
        plots &&
        plots.length > 0 && <Separator />}

      {/* Data Tables */}
      <div>
        <h2 className="text-lg font-semibold mb-4 flex items-center gap-2">
          <BarChart3 className="h-5 w-5" />
          Detailed Data
        </h2>
        {tables && tables.length > 0 ? (
          <TablesDisplay tables={tables} />
        ) : (
          <Card>
            <CardContent className="py-8">
              <div className="text-center">
                <BarChart3 className="h-12 w-12 mx-auto mb-4 text-muted-foreground opacity-50" />
                <p className="text-center text-muted-foreground">
                  No data tables available for this backtest.
                </p>
              </div>
            </CardContent>
          </Card>
        )}
      </div>

      {/* Configuration Summary at the bottom */}
      <Separator />

      <Card>
        <CardHeader>
          <CardTitle>Backtest Configuration</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
            <div>
              <p className="text-sm font-medium text-muted-foreground">
                Bot Type
              </p>
              <p className="text-sm">{backtest.config.type}</p>
            </div>
            <div>
              <p className="text-sm font-medium text-muted-foreground">
                Version
              </p>
              <p className="text-sm">v{backtest.config.version}</p>
            </div>
          </div>

          <Separator className="my-6" />

          <div>
            <h3 className="text-sm font-semibold mb-4">
              Configuration Details
            </h3>
            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
              <ConfigurationSummary config={backtest.config} />
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
