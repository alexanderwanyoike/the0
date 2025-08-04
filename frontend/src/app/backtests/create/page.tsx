'use client';

import React from 'react';
import { useSearchParams, useRouter } from 'next/navigation';
import { withAuth } from '@/components/auth/with-auth';
import DashboardLayout from '@/components/layouts/dashboard-layout';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { ArrowLeft, BarChart3 } from 'lucide-react';
import { BacktestCreationForm } from '@/components/backtests/backtest-creation-form';

function CreateBacktestPage() {
  const searchParams = useSearchParams();
  const router = useRouter();

  const preSelectedBotName = searchParams?.get('name') || undefined;
  const preSelectedBotVersion = searchParams?.get('version') || undefined;

  const handleBack = () => {
    router.back();
  };

  return (
    <DashboardLayout>
      <div className="container max-w-7xl py-6">
        {/* Header */}
        <div className="flex items-center gap-4 mb-6">
          <Button variant="outline" onClick={handleBack} className="gap-2">
            <ArrowLeft className="h-4 w-4" />
            Back
          </Button>
          <div className="flex items-center gap-3">
            <div className="flex items-center justify-center h-10 w-10 rounded-lg bg-primary/10">
              <BarChart3 className="h-5 w-5 text-primary" />
            </div>
            <div>
              <h1 className="text-2xl font-bold">Create Backtest</h1>
              <p className="text-muted-foreground">
                Test your trading strategies with historical data
              </p>
            </div>
          </div>
        </div>

        {/* Two Column Layout on Desktop */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          {/* Left Column - Creation Form */}
          <div className="space-y-6">
            <BacktestCreationForm
              preSelectedBotName={preSelectedBotName}
              preSelectedBotVersion={preSelectedBotVersion}
            />
          </div>

          {/* Right Column - Basic Information */}
          <div className="space-y-6">
            <Card>
              <CardHeader>
                <CardTitle>About Backtesting</CardTitle>
              </CardHeader>
              <CardContent className="space-y-4">
                <div>
                  <h4 className="font-medium mb-2">What is backtesting?</h4>
                  <p className="text-sm text-muted-foreground">
                    Backtesting allows you to test your custom bot&apos;s
                    strategy against historical data to evaluate its performance
                    before deploying it live.
                  </p>
                </div>

                <div>
                  <h4 className="font-medium mb-2">How it works</h4>
                  <ul className="text-sm text-muted-foreground space-y-1">
                    <li>• Select your custom bot and version</li>
                    <li>• Configure the required parameters</li>
                    <li>• Run the simulation with historical data</li>
                    <li>• Analyze the results and metrics</li>
                  </ul>
                </div>

                <div className="p-3 bg-blue-50 dark:bg-blue-950/20 rounded-lg border border-blue-200 dark:border-blue-800">
                  <p className="text-xs text-blue-700 dark:text-blue-300">
                    <strong>Note:</strong> Backtest results are based on your
                    bot&apos;s specific implementation and the parameters you
                    configure. Different bots may require different
                    configuration options.
                  </p>
                </div>
              </CardContent>
            </Card>
          </div>
        </div>
      </div>
    </DashboardLayout>
  );
}

export default withAuth(CreateBacktestPage);
