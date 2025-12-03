import React from "react";
import { useRouter } from "next/navigation";
import { Bot, AlertTriangle } from "lucide-react";
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert";
import { useCustomBots } from "@/hooks/custom-bots/use-custom-bots";
import { EmptyState } from "@/components/custom-bots/empty-state";
import { BotCard } from "@/components/custom-bots/bots-card";
import { LoadingState } from "@/components/custom-bots/loading-state";

const CustomBotsDashboard = () => {
  const router = useRouter();
  const { bots, loading, error } = useCustomBots();

  const handleBotClick = (bot: any) => {
    router.push(`/custom-bots/${bot.name}`);
  };

  if (error) {
    return (
      <div className="container max-w-7xl mx-auto py-6 px-4 lg:px-6">
        <Alert variant="destructive" className="mb-6">
          <AlertTriangle className="h-4 w-4" />
          <AlertTitle>Error Loading Custom Bots</AlertTitle>
          <AlertDescription>{error}</AlertDescription>
        </Alert>
      </div>
    );
  }

  return (
    <div className="container max-w-7xl mx-auto py-6 px-4 lg:px-6">
      {/* Header */}
      <div className="mb-8">
        <div className="flex items-center gap-3 mb-4">
          <div className="flex items-center justify-center h-12 w-12 rounded-lg bg-primary/10">
            <Bot className="h-6 w-6 text-primary" />
          </div>
          <div>
            <h1 className="text-3xl font-bold">Custom Bots</h1>
            <p className="text-muted-foreground">
              Manage your custom trading bots
            </p>
          </div>
        </div>
      </div>

      {/* Loading State */}
      {loading && <LoadingState />}

      {/* Empty State */}
      {!loading && bots.length === 0 && <EmptyState />}

      {/* Bots Grid */}
      {!loading && bots.length > 0 && (
        <div className="grid grid-cols-1 lg:grid-cols-2 2xl:grid-cols-3 gap-8">
          {bots.map((bot: any) => (
            <BotCard
              key={bot.id}
              bot={bot}
              onClick={() => handleBotClick(bot)}
            />
          ))}
        </div>
      )}

      {/* Statistics Footer */}
      {!loading && bots.length > 0 && (
        <div className="mt-12 pt-8 border-t">
          <div className="flex justify-center">
            <div className="text-center">
              <div className="text-2xl font-bold text-primary">
                {bots.length}
              </div>
              <div className="text-sm text-muted-foreground">Total Bots</div>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

export default CustomBotsDashboard;
