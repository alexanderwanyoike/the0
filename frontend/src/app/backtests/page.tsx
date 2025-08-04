'use client';

import { useEffect, useState } from 'react';
import DashboardLayout from '@/components/layouts/dashboard-layout';
import { useAuth } from '@/contexts/auth-context';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/components/ui/table';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import Link from 'next/link';
import moment from 'moment';
import { withAuth } from '@/components/auth/with-auth';
import { useRouter } from 'next/navigation';
import { BarChart3, Clock, Bot, Plus } from 'lucide-react';
import { useBacktests } from '@/hooks/backtests/use-backtests';
import { Backtest } from '@/types/backtest';

// Mobile Backtest Card Component
const BacktestCard = ({ backtest }: { backtest: Backtest }) => {
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

  return (
    <Card className="hover:shadow-md transition-shadow">
      <CardHeader className="pb-3 p-4">
        <div className="space-y-3">
          <div className="flex items-center gap-2">
            <div className="p-1.5 rounded-lg bg-primary/10 flex-shrink-0">
              <BarChart3 className="h-4 w-4 text-primary" />
            </div>
            <div className="min-w-0 flex-1">
              <CardTitle className="text-sm leading-tight">
                <Link
                  href={`/backtests/${backtest.id}`}
                  className="hover:underline block truncate"
                >
                  {backtest.name}
                </Link>
              </CardTitle>
            </div>
          </div>
          <div className="flex flex-wrap gap-1.5">
            <Badge
              variant={getStatusColor(backtest.status)}
              className="text-xs"
            >
              {backtest.status}
            </Badge>
            <Badge variant="outline" className="text-xs truncate">
              {backtest.config.type}
            </Badge>
          </div>
        </div>
      </CardHeader>
      <CardContent className="pt-0 p-4">
        <div className="space-y-3">
          <div className="flex items-center gap-2 text-sm text-muted-foreground">
            <Clock className="h-3 w-3 flex-shrink-0" />
            <span className="truncate text-xs">v{backtest.config.version}</span>
          </div>

          <div className="space-y-2 text-xs">
            <div className="flex justify-between items-start">
              <div className="min-w-0 flex-1">
                <div className="font-medium text-muted-foreground mb-1">
                  Created
                </div>
                <div className="truncate">
                  {moment(backtest.createdAt).format('MMM D, YYYY')}
                </div>
                <div className="text-muted-foreground text-xs">
                  {moment(backtest.createdAt).format('h:mm A')}
                </div>
              </div>
              <div className="min-w-0 flex-1 text-right">
                <div className="font-medium text-muted-foreground mb-1">
                  Updated
                </div>
                <div className="truncate">
                  {moment(backtest.updatedAt).format('MMM D, YYYY')}
                </div>
                <div className="text-muted-foreground text-xs">
                  {moment(backtest.updatedAt).format('h:mm A')}
                </div>
              </div>
            </div>
          </div>
        </div>
      </CardContent>
    </Card>
  );
};

// Empty State Component
const EmptyState = ({ onCreateBacktest }: { onCreateBacktest: () => void }) => (
  <div className="flex flex-col items-center justify-center py-16 px-4">
    <div className="w-20 h-20 bg-muted rounded-full flex items-center justify-center mb-6">
      <BarChart3 className="w-10 h-10 text-muted-foreground" />
    </div>
    <h3 className="text-xl font-semibold mb-2">No backtests yet</h3>
    <p className="text-muted-foreground text-center max-w-md mb-6">
      Get started by creating your first backtest to validate your trading
      strategies with historical data
    </p>
    <Button onClick={onCreateBacktest} className="gap-2">
      <Plus className="w-4 h-4" />
      Create your first backtest
    </Button>
  </div>
);

const BacktestsPage = () => {
  const { backtests, loading, error } = useBacktests();
  const { user } = useAuth();
  const router = useRouter();

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

  const handleCreateBacktest = () => {
    router.push('/backtests/create');
  };

  if (loading) {
    return (
      <DashboardLayout>
        <div className="container mx-auto p-6 max-w-7xl">
          <div className="animate-pulse space-y-6">
            <div className="h-8 bg-muted rounded w-1/4"></div>
            <div className="h-4 bg-muted rounded w-1/6"></div>
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
              {Array.from({ length: 6 }).map((_, i) => (
                <div key={i} className="h-48 bg-muted rounded"></div>
              ))}
            </div>
          </div>
        </div>
      </DashboardLayout>
    );
  }

  if (error) {
    return (
      <DashboardLayout>
        <div className="container mx-auto p-6 max-w-7xl">
          <div className="text-center py-16">
            <p className="text-destructive mb-4">
              Error loading backtests: {error}
            </p>
            <Button onClick={() => window.location.reload()}>Try Again</Button>
          </div>
        </div>
      </DashboardLayout>
    );
  }

  return (
    <DashboardLayout>
      <div className="container mx-auto px-3 sm:px-6 py-4 sm:py-6 max-w-7xl">
        {/* Header */}
        <div className="mb-6 sm:mb-8">
          <div className="flex flex-col gap-3 sm:flex-row sm:items-center sm:justify-between sm:gap-4">
            <div className="min-w-0">
              <h2 className="text-sm font-medium text-muted-foreground">
                Backtests
              </h2>
              <p className="text-xl sm:text-2xl font-semibold">
                {backtests.length}{' '}
                {backtests.length === 1 ? 'backtest' : 'backtests'}
              </p>
            </div>
            <Button
              onClick={handleCreateBacktest}
              className="gap-2 w-full sm:w-auto text-sm"
            >
              <Plus className="w-4 h-4" />
              <span>New Backtest</span>
            </Button>
          </div>
        </div>

        {/* Content */}
        {backtests.length === 0 ? (
          <EmptyState onCreateBacktest={handleCreateBacktest} />
        ) : (
          <>
            {/* Mobile View - Cards */}
            <div className="block lg:hidden">
              <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
                {backtests.map((backtest) => (
                  <BacktestCard key={backtest.id} backtest={backtest} />
                ))}
              </div>
            </div>

            {/* Desktop View - Table */}
            <div className="hidden lg:block">
              <div className="rounded-lg border bg-card">
                <Table>
                  <TableHeader>
                    <TableRow className="hover:bg-transparent">
                      <TableHead className="font-medium">Name</TableHead>
                      <TableHead className="font-medium">Bot Type</TableHead>
                      <TableHead className="font-medium">Version</TableHead>
                      <TableHead className="font-medium">Status</TableHead>
                      <TableHead className="font-medium">Created</TableHead>
                      <TableHead className="font-medium">
                        Last Updated
                      </TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    {backtests.map((backtest) => (
                      <TableRow
                        key={backtest.id}
                        className="hover:bg-muted/50 cursor-pointer"
                      >
                        <TableCell className="font-medium">
                          <Link
                            href={`/backtests/${backtest.id}`}
                            className="hover:underline"
                          >
                            {backtest.name}
                          </Link>
                        </TableCell>
                        <TableCell>
                          <Badge
                            variant="outline"
                            className="font-mono text-xs"
                          >
                            {backtest.config.type}
                          </Badge>
                        </TableCell>
                        <TableCell className="font-mono text-sm">
                          v{backtest.config.version}
                        </TableCell>
                        <TableCell>
                          <Badge
                            variant={getStatusColor(backtest.status)}
                            className="text-xs"
                          >
                            {backtest.status}
                          </Badge>
                        </TableCell>
                        <TableCell className="text-sm">
                          <div className="space-y-1">
                            <div>
                              {moment(backtest.createdAt).format('MMM D, YYYY')}
                            </div>
                            <div className="text-xs text-muted-foreground">
                              {moment(backtest.createdAt).format('h:mm A')}
                            </div>
                          </div>
                        </TableCell>
                        <TableCell className="text-sm">
                          <div className="space-y-1">
                            <div>
                              {moment(backtest.updatedAt).format('MMM D, YYYY')}
                            </div>
                            <div className="text-xs text-muted-foreground">
                              {moment(backtest.updatedAt).format('h:mm A')}
                            </div>
                          </div>
                        </TableCell>
                      </TableRow>
                    ))}
                  </TableBody>
                </Table>
              </div>
            </div>
          </>
        )}
      </div>
    </DashboardLayout>
  );
};

export default withAuth(BacktestsPage);
