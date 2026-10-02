"use client";

import { useEffect, useMemo, useState } from "react";
import { useRouter } from "next/navigation";
import { useAuth } from "@/contexts/auth-context";
import { useToast } from "@/hooks/use-toast";
import { useDashboardBots } from "@/contexts/dashboard-bots-context";
import { BotService, Bot as ApiBotType } from "@/lib/api/api-client";
import { getErrorMessage } from "@/lib/axios";
import { maskSensitiveConfig } from "@/lib/mask-config";

export interface DetailBot extends ApiBotType {
  userId?: string;
  user_id?: string;
  name?: string;
  version?: string;
  customBotId?: string;
}

async function loadOwnedBot(botId: string, userId: string): Promise<DetailBot> {
  const result = await BotService.getBot(botId);
  if (!result.success) {
    throw new Error(result.error.message || "Failed to fetch bot");
  }
  const botData = result.data as DetailBot;
  const botUserId = botData.userId || botData.user_id;
  if (botUserId !== userId) throw new Error("Unauthorized access");
  return botData;
}

const isGoneOrForbidden = (error: unknown) =>
  error instanceof Error &&
  (error.message === "Bot not found" ||
    error.message === "Unauthorized access");

/** Loads the bot for the signed-in owner and exposes its detail actions. */
export function useOwnedBot(botId: string) {
  const [bot, setBot] = useState<DetailBot | null>(null);
  const [loading, setLoading] = useState(true);
  const [isDeleting, setIsDeleting] = useState(false);
  const [isUpdatingEnabled, setIsUpdatingEnabled] = useState(false);
  const router = useRouter();
  const { toast } = useToast();
  const { user } = useAuth();
  const { removeBotFromList, bots } = useDashboardBots();

  useEffect(() => {
    const fetchBot = async () => {
      if (!botId || !user) return;
      setLoading(true);
      try {
        setBot(await loadOwnedBot(botId, user.id));
      } catch (error) {
        console.error("Error fetching bot:", error);
        toast({
          title: "Error",
          description: `Failed to load bot: ${error instanceof Error ? error.message : "Unknown error"}`,
          variant: "destructive",
        });
        if (isGoneOrForbidden(error)) {
          setTimeout(() => router.push("/dashboard"), 2000);
        }
      } finally {
        setLoading(false);
      }
    };
    fetchBot();
  }, [botId, user, router, toast]);

  // Memoized so the deep copy does not run on every render
  const maskedConfig = useMemo(
    () => (bot ? maskSensitiveConfig(bot.config) : null),
    [bot?.config],
  );

  const copyConfig = () => {
    if (!bot) return;
    navigator.clipboard.writeText(
      JSON.stringify(maskSensitiveConfig(bot.config), null, 2),
    );
    toast({
      description: "Bot configuration copied to clipboard",
      duration: 2000,
    });
  };

  const deleteBot = async () => {
    if (!bot) return;
    setIsDeleting(true);
    try {
      const result = await BotService.deleteBot(botId);
      if (!result.success) {
        throw new Error(result.error.message || "Failed to delete bot");
      }
      toast({ description: "Bot deleted successfully", duration: 2000 });
      removeBotFromList(botId);
      const remaining = bots.filter((b) => b.id !== botId);
      router.replace(
        remaining.length > 0 ? `/dashboard/${remaining[0].id}` : "/dashboard",
      );
    } catch (error) {
      console.error("Error deleting bot:", error);
      toast({
        title: "Delete Failed",
        description:
          error instanceof Error ? error.message : "Failed to delete bot",
        variant: "destructive",
      });
      setIsDeleting(false);
    }
  };

  const toggleEnabled = async (enabled: boolean) => {
    if (!bot) return;
    setIsUpdatingEnabled(true);
    try {
      const updatedConfig = { ...bot.config, enabled };
      const result = await BotService.updateBot(botId, updatedConfig);
      if (!result.success) throw new Error(result.error.message);
      setBot((prev) => (prev ? { ...prev, config: updatedConfig } : null));
      toast({
        description: `Bot ${enabled ? "enabled" : "disabled"} successfully. It may take a few moments to reflect the change.`,
        duration: 2000,
      });
    } catch (error) {
      console.error("Error updating bot enabled status:", error);
      toast({
        title: "Update Failed",
        description: getErrorMessage(error),
        variant: "destructive",
      });
    } finally {
      setIsUpdatingEnabled(false);
    }
  };

  return {
    bot,
    loading,
    maskedConfig,
    isDeleting,
    isUpdatingEnabled,
    copyConfig,
    deleteBot,
    toggleEnabled,
  };
}
