'use client';

import React, { useState } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { PlotComponent } from '@/components/ui/plot';
import { PlotData } from '@/types/backtest';
import {
  BarChart3,
  AlertTriangle,
  ChevronLeft,
  ChevronRight,
} from 'lucide-react';

interface PlotsDisplayProps {
  plots: (PlotData | string)[]; // Support both object and stringified formats
}

const UNTITLED_CHART_TITLE = 'Untitled Chart';
const CHARTS_PER_PAGE = 10;

// Utility function to process plot data from different formats
const processPlotData = (plot: PlotData | string): PlotData | null => {
  try {
    // Handle string format - parse JSON
    if (typeof plot === 'string') {
      const parsed = JSON.parse(plot);
      if (!parsed.data) {
        console.error('Plot data missing required "data" property');
        return null;
      }
      return {
        title:
          parsed.title ||
          parsed.layout?.title?.text ||
          parsed.layout?.title ||
          UNTITLED_CHART_TITLE,
        data: parsed.data,
        layout: { ...parsed.layout, title: undefined },
        config: parsed.config,
      } as PlotData;
    }

    // Handle object format - validate required properties
    if (!plot.data) {
      console.error('Plot data missing required "data" property');
      return null;
    }

    // Return with fallback title
    return {
      ...plot,
      title:
        plot.title ||
        plot.layout?.title?.text ||
        plot.layout?.title ||
        UNTITLED_CHART_TITLE,
      layout: { ...plot.layout, title: undefined },
    };
  } catch (error) {
    console.error('Failed to parse plot data:', error);
    return null;
  }
};

// Component to display plot errors
const PlotError = ({ title, error }: { title?: string; error: string }) => (
  <div className="p-6 text-center">
    <AlertTriangle className="h-12 w-12 mx-auto mb-4 text-amber-500" />
    <h4 className="font-medium mb-2">{title || 'Chart Error'}</h4>
    <p className="text-sm text-muted-foreground">{error}</p>
  </div>
);

export function PlotsDisplay({ plots }: PlotsDisplayProps) {
  const [selectedChart, setSelectedChart] = useState(0);
  const [currentPage, setCurrentPage] = useState(0);

  // Auto-navigate to page containing selected chart - moved before early returns
  React.useEffect(() => {
    if (!plots || plots.length === 0) return;

    const processedResults = plots.map((plot, index) => ({
      index,
      original: plot,
      processed: processPlotData(plot),
    }));

    const validPlots = processedResults.filter(
      (result) => result.processed !== null,
    );

    if (validPlots.length === 0) return;

    const plotsToDisplay = validPlots.map((result) => result.processed!);
    const totalPages = Math.ceil(plotsToDisplay.length / CHARTS_PER_PAGE);
    const pageContainingSelection = Math.floor(selectedChart / CHARTS_PER_PAGE);

    if (
      pageContainingSelection !== currentPage &&
      pageContainingSelection < totalPages
    ) {
      setCurrentPage(pageContainingSelection);
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [selectedChart, plots]);

  if (!plots || plots.length === 0) {
    return (
      <Card>
        <CardContent className="py-8">
          <div className="text-center">
            <BarChart3 className="h-12 w-12 mx-auto mb-4 text-muted-foreground opacity-50" />
            <p className="text-center text-muted-foreground">
              No charts available for this backtest.
            </p>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Process plots and separate valid from invalid ones
  const processedResults = plots.map((plot, index) => ({
    index,
    original: plot,
    processed: processPlotData(plot),
  }));

  const validPlots = processedResults.filter(
    (result) => result.processed !== null,
  );
  const invalidPlots = processedResults.filter(
    (result) => result.processed === null,
  );

  // If no valid plots, show error state
  if (validPlots.length === 0) {
    return (
      <Card>
        <CardContent className="py-8">
          <PlotError
            title="Chart Data Error"
            error="All chart data is malformed or missing required properties."
          />
          {invalidPlots.length > 0 && (
            <Alert className="mt-4">
              <AlertTriangle className="h-4 w-4" />
              <AlertDescription>
                {invalidPlots.length} chart{invalidPlots.length > 1 ? 's' : ''}{' '}
                could not be displayed due to formatting issues.
              </AlertDescription>
            </Alert>
          )}
        </CardContent>
      </Card>
    );
  }

  const plotsToDisplay = validPlots.map((result) => result.processed!);

  // Pagination for chart selection
  const totalPages = Math.ceil(plotsToDisplay.length / CHARTS_PER_PAGE);
  const startIndex = currentPage * CHARTS_PER_PAGE;
  const currentPageCharts = plotsToDisplay.slice(
    startIndex,
    startIndex + CHARTS_PER_PAGE,
  );

  const selectedPlot = plotsToDisplay[selectedChart];

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <BarChart3 className="h-5 w-5" />
          Charts
          {invalidPlots.length > 0 && (
            <span className="text-sm text-muted-foreground">
              ({validPlots.length} of {plots.length})
            </span>
          )}
        </CardTitle>
      </CardHeader>
      <CardContent>
        {invalidPlots.length > 0 && (
          <Alert className="mb-4">
            <AlertTriangle className="h-4 w-4" />
            <AlertDescription>
              {invalidPlots.length} chart{invalidPlots.length > 1 ? 's' : ''}{' '}
              could not be displayed due to formatting issues.
            </AlertDescription>
          </Alert>
        )}

        <div className="grid grid-cols-1 lg:grid-cols-4 gap-8">
          {/* Chart Selection Panel */}
          <div className="lg:col-span-1">
            <div className="flex items-center justify-between mb-6">
              <h3 className="text-sm font-medium text-foreground">Charts</h3>
              {totalPages > 1 && (
                <div className="flex items-center gap-2">
                  <Button
                    variant="ghost"
                    size="sm"
                    onClick={() => setCurrentPage(Math.max(0, currentPage - 1))}
                    disabled={currentPage === 0}
                    className="h-8 w-8 p-0"
                  >
                    <ChevronLeft className="h-4 w-4" />
                  </Button>
                  <span className="text-xs text-muted-foreground min-w-[40px] text-center">
                    {currentPage + 1} / {totalPages}
                  </span>
                  <Button
                    variant="ghost"
                    size="sm"
                    onClick={() =>
                      setCurrentPage(Math.min(totalPages - 1, currentPage + 1))
                    }
                    disabled={currentPage === totalPages - 1}
                    className="h-8 w-8 p-0"
                  >
                    <ChevronRight className="h-4 w-4" />
                  </Button>
                </div>
              )}
            </div>

            <div className="space-y-1">
              {currentPageCharts.map((plot, pageIndex) => {
                const globalIndex = startIndex + pageIndex;
                return (
                  <button
                    key={globalIndex}
                    onClick={() => setSelectedChart(globalIndex)}
                    className={`w-full text-left px-3 py-2 rounded-lg transition-all duration-200 ${
                      selectedChart === globalIndex
                        ? 'bg-primary/10 text-primary border-l-3 border-primary pl-3'
                        : 'text-foreground hover:bg-accent'
                    }`}
                  >
                    <div className="flex items-center gap-3">
                      <BarChart3
                        className={`h-4 w-4 flex-shrink-0 ${
                          selectedChart === globalIndex
                            ? 'text-primary'
                            : 'text-muted-foreground'
                        }`}
                      />
                      <span className="text-sm truncate">{plot.title}</span>
                    </div>
                  </button>
                );
              })}
            </div>
          </div>

          {/* Chart Display Panel */}
          <div className="lg:col-span-3">
            <div className="mb-4">
              <h3 className="text-lg font-normal text-foreground">
                {selectedPlot.title}
              </h3>
            </div>
            <div className="h-[500px] w-full">
              <PlotComponent
                data={selectedPlot.data}
                layout={{
                  margin: { t: 20, l: 60, r: 40, b: 60 },
                  paper_bgcolor: 'transparent',
                  plot_bgcolor: 'transparent',
                  font: {
                    family: 'var(--font-sans), system-ui, sans-serif',
                    size: 12,
                    color: 'hsl(var(--foreground))',
                  },
                  xaxis: {
                    title: {
                      text: 'Time',
                      font: { color: 'hsl(var(--muted-foreground))' },
                    },
                    tickfont: { color: 'hsl(var(--muted-foreground))' },
                    gridcolor: 'hsl(var(--border))',
                    linecolor: 'hsl(var(--border))',
                    zerolinecolor: 'hsl(var(--border))',
                  },
                  yaxis: {
                    title: {
                      text: 'Value',
                      font: { color: 'hsl(var(--muted-foreground))' },
                    },
                    tickfont: { color: 'hsl(var(--muted-foreground))' },
                    gridcolor: 'hsl(var(--border))',
                    linecolor: 'hsl(var(--border))',
                    zerolinecolor: 'hsl(var(--border))',
                  },
                  showlegend: true,
                  legend: {
                    font: { color: 'hsl(var(--foreground))' },
                    bgcolor: 'transparent',
                    bordercolor: 'transparent',
                  },
                  hovermode: 'x unified',
                  hoverlabel: {
                    bgcolor: 'hsl(var(--popover))',
                    bordercolor: 'hsl(var(--border))',
                    font: { color: 'hsl(var(--popover-foreground))' },
                  },
                  autosize: true,
                  // Spread layout but ensure our color settings take precedence
                  ...selectedPlot.layout,
                }}
                config={{
                  displayModeBar: true,
                  displaylogo: false,
                  modeBarButtonsToRemove: ['pan2d', 'lasso2d', 'select2d'],
                  toImageButtonOptions: {
                    format: 'png',
                    filename: `backtest_${selectedPlot.title.toLowerCase().replace(/\s+/g, '_')}`,
                    height: 600,
                    width: 1000,
                    scale: 1,
                  },
                  responsive: true,
                  ...selectedPlot.config,
                }}
              />
            </div>
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
