'use client';
import React from 'react';
import { useEffect } from 'react';
import { useRouter } from 'next/navigation';
import DashboardLayout from '@/components/layouts/dashboard-layout';
import { Alert, AlertDescription, AlertTitle } from '@/components/ui/alert';
import { Button } from '@/components/ui/button';
import { AlertTriangle, ArrowLeft, RefreshCw } from 'lucide-react';

interface BotDetailErrorProps {
  error: Error & { digest?: string };
  reset: () => void;
}

export default function BotDetailError({ error, reset }: BotDetailErrorProps) {
  const router = useRouter();

  useEffect(() => {
    console.error('Custom Bot detail page error:', error);
  }, [error]);

  return (
    <DashboardLayout>
      <div className="container max-w-7xl mx-auto py-6 px-4 lg:px-6">
        <Alert variant="destructive" className="mb-6">
          <AlertTriangle className="h-4 w-4" />
          <AlertTitle>Failed to Load Bot Details</AlertTitle>
          <AlertDescription className="mt-2">
            <p className="mb-4">
              We couldn&#39;t load the details for this bot. This might be
              because:
            </p>
            <ul className="list-disc list-inside mb-4 space-y-1 text-sm">
              <li>The bot doesn&#39;t exist or was deleted</li>
              <li>You don&#39;t have permission to view this bot</li>
              <li>There was a network or server error</li>
            </ul>
            <div className="flex gap-2">
              <Button
                variant="outline"
                size="sm"
                onClick={() => router.push('/custom-bots')}
                className="gap-2"
              >
                <ArrowLeft className="h-4 w-4" />
                Back to Custom Bots
              </Button>
              <Button
                variant="outline"
                size="sm"
                onClick={reset}
                className="gap-2"
              >
                <RefreshCw className="h-4 w-4" />
                Try again
              </Button>
            </div>
          </AlertDescription>
        </Alert>
      </div>
    </DashboardLayout>
  );
}
