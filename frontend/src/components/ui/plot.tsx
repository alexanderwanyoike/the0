"use client";

import dynamic from "next/dynamic";
import { useTheme } from "next-themes";
import { useEffect, useState } from "react";
import { Skeleton } from "@/components/ui/skeleton";

const Plot = dynamic(() => import("react-plotly.js"), {
  ssr: false,
  loading: () => <Skeleton className="h-96 w-full" />,
});

interface PlotProps {
  data: any[];
  layout?: any;
  config?: any;
  className?: string;
}

// Theme-aware color scheme generator
function getPlotTheme(isDark: boolean) {
  if (isDark) {
    return {
      paper_bgcolor: "hsl(142.8 10% 5%)", // --background dark
      plot_bgcolor: "hsl(142.8 5% 3%)", // --card dark
      font: { color: "hsl(142.8 5% 90%)" }, // --foreground dark
      colorway: [
        "hsl(142.8 64.2% 24.1%)", // --primary
        "hsl(142.8 50% 35%)", // secondary green
        "hsl(104.8 50% 35%)", // accent green
        "hsl(60 50% 35%)", // yellow-green
        "hsl(220 50% 35%)", // blue
        "hsl(320 50% 35%)", // purple
        "hsl(20 50% 35%)", // orange
        "hsl(0 50% 35%)", // red
      ],
      xaxis: {
        gridcolor: "hsl(142.8 20% 18%)", // --border dark
        linecolor: "hsl(142.8 20% 18%)",
        tickcolor: "hsl(142.8 20% 18%)",
        color: "hsl(142.8 5% 90%)",
      },
      yaxis: {
        gridcolor: "hsl(142.8 20% 18%)", // --border dark
        linecolor: "hsl(142.8 20% 18%)",
        tickcolor: "hsl(142.8 20% 18%)",
        color: "hsl(142.8 5% 90%)",
      },
    };
  } else {
    return {
      paper_bgcolor: "hsl(142.8 5% 95%)", // --background light
      plot_bgcolor: "hsl(142.8 5% 90%)", // --card light
      font: { color: "hsl(142.8 5% 3%)" }, // --foreground light
      colorway: [
        "hsl(142.8 64.2% 24.1%)", // --primary
        "hsl(142.8 50% 40%)", // secondary green
        "hsl(104.8 50% 40%)", // accent green
        "hsl(60 50% 40%)", // yellow-green
        "hsl(220 50% 40%)", // blue
        "hsl(320 50% 40%)", // purple
        "hsl(20 50% 40%)", // orange
        "hsl(0 50% 40%)", // red
      ],
      xaxis: {
        gridcolor: "hsl(142.8 20% 50%)", // --border light
        linecolor: "hsl(142.8 20% 50%)",
        tickcolor: "hsl(142.8 20% 50%)",
        color: "hsl(142.8 5% 3%)",
      },
      yaxis: {
        gridcolor: "hsl(142.8 20% 50%)", // --border light
        linecolor: "hsl(142.8 20% 50%)",
        tickcolor: "hsl(142.8 20% 50%)",
        color: "hsl(142.8 5% 3%)",
      },
    };
  }
}

export function PlotComponent({ data, layout, config, className }: PlotProps) {
  const { theme, resolvedTheme } = useTheme();
  const [mounted, setMounted] = useState(false);

  // Ensure component is mounted to prevent hydration mismatch
  useEffect(() => {
    setMounted(true);
  }, []);

  if (!mounted) {
    return <Skeleton className="h-96 w-full" />;
  }

  const isDark = resolvedTheme === "dark";
  const plotTheme = getPlotTheme(isDark);

  return (
    <div className={className}>
      <Plot
        data={data}
        layout={{
          autosize: true,
          margin: { t: 20, l: 40, r: 40, b: 40 },
          ...plotTheme,
          ...layout,
        }}
        config={{
          responsive: true,
          displayModeBar: false,
          ...config,
        }}
        style={{ width: "100%", height: "100%" }}
        useResizeHandler={true}
      />
    </div>
  );
}
