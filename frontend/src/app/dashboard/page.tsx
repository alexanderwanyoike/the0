'use client';
import { useEffect, useState } from 'react';
import { BotService, Bot as ApiBot } from '@/lib/api/api-client';
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
import cronstrue from 'cronstrue';
import moment from 'moment';
import { withAuth } from '@/components/auth/with-auth';
import { useRouter } from 'next/navigation';
import { Bot, Calendar, Clock, ExternalLink } from 'lucide-react';

interface BotConfig {
  type: string;
  symbol: string;
  schedule?: string;
  name: string;
}

// Use the API Bot type and extend it with local needs
interface DashboardBot extends ApiBot {
  config: BotConfig;
  userId?: string;
}

// Mobile Bot Card Component
const BotCard = ({ bot }: { bot: ApiBot }) => {
  const getReadableSchedule = (schedule: string | undefined) => {
    if (!schedule) return 'Real-time';
    try {
      return cronstrue.toString(schedule);
    } catch {
      return schedule;
    }
  };

  return (
    <Card className="hover:shadow-md transition-shadow">
      <CardHeader className="pb-3 p-4">
        <div className="space-y-3">
          <div className="flex items-center gap-2">
            <div className="p-1.5 rounded-lg bg-primary/10 flex-shrink-0">
              <Bot className="h-4 w-4 text-primary" />
            </div>
            <div className="min-w-0 flex-1">
              <CardTitle className="text-sm leading-tight">
                <Link
                  href={`/bot/${bot.id}`}
                  className="hover:underline block truncate"
                >
                  {(bot.config as any)?.name || bot.id}
                </Link>
              </CardTitle>
            </div>
          </div>
          <div className="flex flex-wrap gap-1.5">
            <Badge variant="secondary" className="text-xs font-mono">
              {(bot.config as any)?.symbol || 'N/A'}
            </Badge>
            <Badge variant="outline" className="text-xs truncate">
              {(bot.config as any)?.type || 'Bot'}
            </Badge>
          </div>
        </div>
      </CardHeader>
      <CardContent className="pt-0 p-4">
        <div className="space-y-3">
          <div className="flex items-center gap-2 text-sm text-muted-foreground">
            <Clock className="h-3 w-3 flex-shrink-0" />
            <span className="truncate text-xs">
              {getReadableSchedule((bot.config as any)?.schedule)}
            </span>
          </div>

          <div className="space-y-2 text-xs">
            <div className="flex justify-between items-start">
              <div className="min-w-0 flex-1">
                <div className="font-medium text-muted-foreground mb-1">
                  Created
                </div>
                <div className="truncate">
                  {moment(bot.createdAt).format('MMM D, YYYY')}
                </div>
                <div className="text-muted-foreground text-xs">
                  {moment(bot.createdAt).format('h:mm A')}
                </div>
              </div>
              <div className="min-w-0 flex-1 text-right">
                <div className="font-medium text-muted-foreground mb-1">
                  Updated
                </div>
                <div className="truncate">
                  {moment(bot.updatedAt).format('MMM D, YYYY')}
                </div>
                <div className="text-muted-foreground text-xs">
                  {moment(bot.updatedAt).format('h:mm A')}
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
const EmptyState = () => (
  <div className="flex flex-col items-center justify-center py-16 px-4">
    <div className="w-20 h-20 bg-muted rounded-full flex items-center justify-center mb-6">
      <Bot className="w-10 h-10 text-muted-foreground" />
    </div>
    <h3 className="text-xl font-semibold mb-2">No trading bots yet</h3>
    <p className="text-muted-foreground text-center max-w-md mb-6">
      Get started by viewing your custom bots or checking your deployed bots
    </p>
    <div className="flex flex-col sm:flex-row gap-3 w-full max-w-md">
      <Button asChild variant="outline" className="gap-2 flex-1">
        <Link href="/user-bots">
          <Bot className="w-4 h-4" />
          View My Bots
        </Link>
      </Button>
      <Button asChild variant="outline" className="gap-2 flex-1">
        <Link href="/custom-bots">
          <Calendar className="w-4 h-4" />
          Custom Bots
        </Link>
      </Button>
    </div>
  </div>
);

const Dashboard = () => {
  const [bots, setBots] = useState<ApiBot[]>([]);
  const [loading, setLoading] = useState(true);
  const { user } = useAuth();
  const router = useRouter();

  useEffect(() => {
    const fetchBots = async () => {
      if (!user) {
        setLoading(false);
        return;
      }
      
      setLoading(true);
      try {
        const result = await BotService.getBots();
        if (result.success) {
          // API should already filter by user, no need for client-side filtering
          setBots(result.data);
        } else {
          console.error('Error fetching bots:', result.error);
          // Add user-friendly error handling
          if (result.error.statusCode === 401) {
            // Handle unauthorized - user needs to log in again
            console.warn('User unauthorized, redirecting to login');
          } else if (result.error.statusCode === 403) {
            console.warn('User forbidden from accessing bots');
          }
        }
      } catch (error) {
        console.error('Error fetching bots:', error);
      } finally {
        setLoading(false);
      }
    };

    fetchBots();
  }, [user]);

  const getReadableSchedule = (schedule: string | undefined) => {
    if (!schedule) return 'Real-time';
    try {
      return cronstrue.toString(schedule);
    } catch {
      return schedule;
    }
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

  return (
    <DashboardLayout>
      <div className="container mx-auto px-3 sm:px-6 py-4 sm:py-6 max-w-7xl">
        {/* Header */}
        <div className="mb-6 sm:mb-8">
          <div className="flex flex-col gap-3 sm:flex-row sm:items-center sm:justify-between sm:gap-4">
            <div className="min-w-0">
              <h2 className="text-sm font-medium text-muted-foreground">
                Trading Bots
              </h2>
              <p className="text-xl sm:text-2xl font-semibold">
                {bots.length} {bots.length === 1 ? 'bot' : 'bots'} running
              </p>
            </div>
          </div>
        </div>

        {/* Content */}
        {bots.length === 0 ? (
          <EmptyState />
        ) : (
          <>
            {/* Mobile View - Cards */}
            <div className="block lg:hidden">
              <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
                {bots.map((bot) => (
                  <BotCard key={bot.id} bot={bot} />
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
                      <TableHead className="font-medium">Symbol</TableHead>
                      <TableHead className="font-medium">Type</TableHead>
                      <TableHead className="font-medium">Schedule</TableHead>
                      <TableHead className="font-medium">Created</TableHead>
                      <TableHead className="font-medium">
                        Last Updated
                      </TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    {bots.map((bot) => (
                      <TableRow
                        key={bot.id}
                        className="hover:bg-muted/50 cursor-pointer"
                      >
                        <TableCell className="font-medium">
                          <Link
                            href={`/bot/${bot.id}`}
                            className="hover:underline"
                          >
                            {(bot.config as any)?.name || bot.id}
                          </Link>
                        </TableCell>
                        <TableCell className="font-mono text-sm">
                          {(bot.config as any)?.symbol || 'N/A'}
                        </TableCell>
                        <TableCell>
                          <Badge
                            variant="outline"
                            className="font-mono text-xs"
                          >
                            {(bot.config as any)?.type || 'Bot'}
                          </Badge>
                        </TableCell>
                        <TableCell className="text-sm">
                          {getReadableSchedule((bot.config as any)?.schedule)}
                        </TableCell>
                        <TableCell className="text-sm">
                          <div className="space-y-1">
                            <div>
                              {moment(bot.createdAt).format(
                                'MMM D, YYYY',
                              )}
                            </div>
                            <div className="text-xs text-muted-foreground">
                              {moment(bot.createdAt).format('h:mm A')}
                            </div>
                          </div>
                        </TableCell>
                        <TableCell className="text-sm">
                          <div className="space-y-1">
                            <div>
                              {moment(bot.updatedAt).format(
                                'MMM D, YYYY',
                              )}
                            </div>
                            <div className="text-xs text-muted-foreground">
                              {moment(bot.updatedAt).format('h:mm A')}
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

export default withAuth(Dashboard);
