"use client";

import React, { useState } from "react";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { ChevronLeft, ChevronRight } from "lucide-react";

interface MetricsSummaryProps {
  metrics: Record<string, any>;
}

const MAX_METRICS_PER_PAGE = 9;

export function MetricsSummary({ metrics }: MetricsSummaryProps) {
  const [currentPage, setCurrentPage] = useState(0);

  // Helper function to format labels
  const formatLabel = (key: string): string => {
    return key
      .replace(/_/g, " ")
      .replace(/([A-Z])/g, " $1")
      .trim()
      .split(" ")
      .map((word) => word.charAt(0).toUpperCase() + word.slice(1).toLowerCase())
      .join(" ");
  };

  // Helper function to format values
  const formatValue = (key: string, value: any): string => {
    if (value === null || value === undefined) return "-";

    // Handle arrays
    if (Array.isArray(value)) {
      return `[${value.length} items]`;
    }

    // Handle objects
    if (typeof value === "object") {
      return `{${Object.keys(value).length} fields}`;
    }

    // Handle numbers
    if (typeof value === "number") {
      const keyLower = key.toLowerCase();

      // Percentage values
      if (
        keyLower.includes("rate") ||
        keyLower.includes("return") ||
        keyLower.includes("pct") ||
        keyLower.includes("ratio")
      ) {
        return `${(value * 100).toFixed(2)}%`;
      }

      // Currency values
      if (
        keyLower.includes("price") ||
        keyLower.includes("value") ||
        keyLower.includes("pnl") ||
        keyLower.includes("profit")
      ) {
        return `$${value.toFixed(2)}`;
      }

      // Regular numbers
      return value % 1 === 0 ? value.toString() : value.toFixed(4);
    }

    return String(value);
  };

  // Helper function to determine if value should be highlighted
  const shouldHighlightValue = (key: string, value: any): boolean => {
    if (typeof value !== "number") return false;

    const keyLower = key.toLowerCase();

    // Highlight returns, profits, and ratios
    return (
      keyLower.includes("return") ||
      keyLower.includes("profit") ||
      keyLower.includes("ratio") ||
      keyLower.includes("pnl")
    );
  };

  const metricEntries = Object.entries(metrics);
  const totalPages = Math.ceil(metricEntries.length / MAX_METRICS_PER_PAGE);
  const startIndex = currentPage * MAX_METRICS_PER_PAGE;
  const currentMetrics = metricEntries.slice(
    startIndex,
    startIndex + MAX_METRICS_PER_PAGE,
  );

  if (metricEntries.length === 0) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Performance Metrics</CardTitle>
        </CardHeader>
        <CardContent>
          <p className="text-sm text-muted-foreground">
            No metrics available for this backtest.
          </p>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <div className="flex items-center justify-between">
          {totalPages > 1 && (
            <div className="flex items-center gap-2">
              <Button
                variant="outline"
                size="sm"
                onClick={() => setCurrentPage(Math.max(0, currentPage - 1))}
                disabled={currentPage === 0}
              >
                <ChevronLeft className="h-4 w-4" />
              </Button>
              <span className="text-xs text-muted-foreground">
                {currentPage + 1} of {totalPages}
              </span>
              <Button
                variant="outline"
                size="sm"
                onClick={() =>
                  setCurrentPage(Math.min(totalPages - 1, currentPage + 1))
                }
                disabled={currentPage === totalPages - 1}
              >
                <ChevronRight className="h-4 w-4" />
              </Button>
            </div>
          )}
        </div>
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6 justify-items-center">
          <dl className="space-y-3 sm:space-y-4 w-full max-w-sm">
            {currentMetrics
              .slice(0, Math.ceil(currentMetrics.length / 3))
              .map(([key, value]) => (
                <div key={key} className="grid grid-cols-3 gap-1">
                  <dt className="text-sm text-muted-foreground">
                    {formatLabel(key)}
                  </dt>
                  <dd className="col-span-2">
                    <code className="px-2 py-1 rounded bg-muted text-xs font-mono">
                      {formatValue(key, value)}
                    </code>
                  </dd>
                </div>
              ))}
          </dl>
          <dl className="space-y-3 sm:space-y-4 w-full max-w-sm">
            {currentMetrics
              .slice(
                Math.ceil(currentMetrics.length / 3),
                Math.ceil((currentMetrics.length * 2) / 3),
              )
              .map(([key, value]) => (
                <div key={key} className="grid grid-cols-3 gap-1">
                  <dt className="text-sm text-muted-foreground">
                    {formatLabel(key)}
                  </dt>
                  <dd className="col-span-2">
                    <code className="px-2 py-1 rounded bg-muted text-xs font-mono">
                      {formatValue(key, value)}
                    </code>
                  </dd>
                </div>
              ))}
          </dl>
          <dl className="space-y-3 sm:space-y-4 w-full max-w-sm">
            {currentMetrics
              .slice(Math.ceil((currentMetrics.length * 2) / 3))
              .map(([key, value]) => (
                <div key={key} className="grid grid-cols-3 gap-1">
                  <dt className="text-sm text-muted-foreground">
                    {formatLabel(key)}
                  </dt>
                  <dd className="col-span-2">
                    <code className="px-2 py-1 rounded bg-muted text-xs font-mono">
                      {formatValue(key, value)}
                    </code>
                  </dd>
                </div>
              ))}
          </dl>
        </div>

        {totalPages > 1 && (
          <div className="flex justify-center mt-6">
            <div className="flex items-center gap-1">
              {Array.from({ length: Math.min(totalPages, 5) }, (_, i) => {
                const pageIndex =
                  totalPages <= 5
                    ? i
                    : currentPage < 3
                      ? i
                      : currentPage > totalPages - 3
                        ? totalPages - 5 + i
                        : currentPage - 2 + i;

                return (
                  <Button
                    key={pageIndex}
                    variant={pageIndex === currentPage ? "default" : "outline"}
                    size="sm"
                    className="w-8 h-8 p-0 text-xs"
                    onClick={() => setCurrentPage(pageIndex)}
                  >
                    {pageIndex + 1}
                  </Button>
                );
              })}
            </div>
          </div>
        )}
      </CardContent>
    </Card>
  );
}
