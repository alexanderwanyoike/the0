"use client";

import React, { useState } from "react";
import { useParams, useRouter } from "next/navigation";
import { withAuth } from "@/components/auth/with-auth";
import DashboardLayout from "@/components/layouts/dashboard-layout";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { Alert, AlertDescription } from "@/components/ui/alert";
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
} from "@/components/ui/alert-dialog";
import {
  ArrowLeft,
  BarChart3,
  Clock,
  CalendarDays,
  Loader2,
  RefreshCcw,
  Trash2,
  AlertCircle,
} from "lucide-react";
import { useBacktest } from "@/hooks/backtests/use-backtest";
import { BacktestResults } from "@/components/backtests/backtest-results";
import moment from "moment";
import { authFetch } from "@/lib/auth-fetch";

function BacktestDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [isDeleting, setIsDeleting] = useState(false);

  const id = params?.id as string;
  const { backtest, loading, error, refetch } = useBacktest(id);

  const handleBack = () => {
    router.push("/backtests");
  };

  const handleDelete = async () => {
    if (!backtest) return;

    setIsDeleting(true);
    try {
      await authFetch(`/api/backtests/${backtest.id}`, {
        method: "DELETE",
      });

      router.push("/backtests");
    } catch (err) {
      console.error("Failed to delete backtest:", err);
    } finally {
      setIsDeleting(false);
    }
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case "completed":
        return "default";
      case "running":
        return "secondary";
      case "pending":
        return "outline";
      case "failed":
        return "destructive";
      default:
        return "outline";
    }
  };

  if (!id) {
    return (
      <DashboardLayout>
        <div className="container max-w-7xl py-6">
          <Alert variant="destructive">
            <AlertCircle className="h-4 w-4" />
            <AlertDescription>Invalid backtest ID.</AlertDescription>
          </Alert>
        </div>
      </DashboardLayout>
    );
  }

  if (loading) {
    return (
      <DashboardLayout>
        <div className="container max-w-7xl py-6">
          <div className="flex items-center justify-center py-12">
            <Loader2 className="h-8 w-8 animate-spin mr-2" />
            <span>Loading backtest details...</span>
          </div>
        </div>
      </DashboardLayout>
    );
  }

  if (error) {
    return (
      <DashboardLayout>
        <div className="container max-w-7xl py-6">
          <div className="space-y-6">
            {/* Header with Back Button */}
            <div className="flex items-center gap-4">
              <Button variant="outline" onClick={handleBack} className="gap-2">
                <ArrowLeft className="h-4 w-4" />
                Back to Backtests
              </Button>
            </div>

            <Alert variant="destructive">
              <AlertCircle className="h-4 w-4" />
              <AlertDescription>{error}</AlertDescription>
            </Alert>

            <div className="flex justify-center">
              <Button onClick={refetch} variant="outline" className="gap-2">
                <RefreshCcw className="h-4 w-4" />
                Try Again
              </Button>
            </div>
          </div>
        </div>
      </DashboardLayout>
    );
  }

  if (!backtest) {
    return (
      <DashboardLayout>
        <div className="container max-w-7xl py-6">
          <Alert variant="destructive">
            <AlertCircle className="h-4 w-4" />
            <AlertDescription>Backtest not found.</AlertDescription>
          </Alert>
        </div>
      </DashboardLayout>
    );
  }

  return (
    <DashboardLayout>
      <div className="container max-w-7xl py-6">
        {/* Header */}
        <div className="flex items-center justify-between mb-6">
          <div className="flex items-center gap-4">
            <Button variant="outline" onClick={handleBack} className="gap-2">
              <ArrowLeft className="h-4 w-4" />
              Back
            </Button>
            <div className="flex items-center gap-3">
              <div className="flex items-center justify-center h-10 w-10 rounded-lg bg-primary/10">
                <BarChart3 className="h-5 w-5 text-primary" />
              </div>
              <div>
                <h1 className="text-2xl font-bold">{backtest.name}</h1>
                <div className="flex items-center gap-2">
                  <Badge variant={getStatusColor(backtest.status)}>
                    {backtest.status}
                  </Badge>
                  <span className="text-muted-foreground">•</span>
                  <span className="text-sm text-muted-foreground">
                    {backtest.config.type} v{backtest.config.version}
                  </span>
                </div>
              </div>
            </div>
          </div>

          <div className="flex items-center gap-2">
            {(backtest.status === "running" ||
              backtest.status === "pending") && (
              <Button
                onClick={refetch}
                variant="outline"
                size="sm"
                className="gap-2"
              >
                <RefreshCcw className="h-4 w-4" />
                Refresh
              </Button>
            )}
            <AlertDialog>
              <AlertDialogTrigger asChild>
                <Button
                  variant="outline"
                  size="sm"
                  className="gap-2 text-red-600 hover:text-red-700"
                >
                  <Trash2 className="h-4 w-4" />
                  Delete
                </Button>
              </AlertDialogTrigger>
              <AlertDialogContent>
                <AlertDialogHeader>
                  <AlertDialogTitle>Delete Backtest</AlertDialogTitle>
                  <AlertDialogDescription>
                    Are you sure you want to delete this backtest? This action
                    cannot be undone.
                  </AlertDialogDescription>
                </AlertDialogHeader>
                <AlertDialogFooter>
                  <AlertDialogCancel>Cancel</AlertDialogCancel>
                  <AlertDialogAction
                    onClick={handleDelete}
                    disabled={isDeleting}
                    className="bg-red-600 hover:bg-red-700"
                  >
                    {isDeleting ? (
                      <>
                        <Loader2 className="h-4 w-4 animate-spin mr-2" />
                        Deleting...
                      </>
                    ) : (
                      "Delete"
                    )}
                  </AlertDialogAction>
                </AlertDialogFooter>
              </AlertDialogContent>
            </AlertDialog>
          </div>
        </div>

        {/* Backtest Info Card */}
        <Card className="mb-6">
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <CalendarDays className="h-5 w-5" />
              Backtest Information
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">
              <div>
                <p className="text-sm font-medium text-muted-foreground">
                  Created
                </p>
                <p className="text-sm">
                  {moment(backtest.createdAt).format("MMM D, YYYY h:mm A")}
                </p>
              </div>
              <div>
                <p className="text-sm font-medium text-muted-foreground">
                  Last Updated
                </p>
                <p className="text-sm">
                  {moment(backtest.updatedAt).format("MMM D, YYYY h:mm A")}
                </p>
              </div>
              <div>
                <p className="text-sm font-medium text-muted-foreground">
                  Duration
                </p>
                <p className="text-sm">
                  {moment(backtest.updatedAt).from(backtest.createdAt, true)}
                </p>
              </div>
              <div>
                <p className="text-sm font-medium text-muted-foreground">
                  Status
                </p>
                <div className="flex items-center gap-2">
                  <Badge variant={getStatusColor(backtest.status)}>
                    {backtest.status}
                  </Badge>
                  {(backtest.status === "running" ||
                    backtest.status === "pending") && (
                    <Clock className="h-3 w-3 text-muted-foreground" />
                  )}
                </div>
              </div>
            </div>
          </CardContent>
        </Card>

        {/* Results Section */}
        <BacktestResults backtest={backtest} />
      </div>
    </DashboardLayout>
  );
}

export default withAuth(BacktestDetailPage);
