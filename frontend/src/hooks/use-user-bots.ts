import { useState, useEffect, useCallback } from 'react';
import { useAuth } from '@/contexts/auth-context';
import { authFetch } from '@/lib/auth-fetch';
import { CustomBotConfig, CustomBotWithVersions } from '@/types/custom-bots';

export interface CustomBotVersion {
  version: string;
  config: CustomBotConfig;
  userId: string;
  id: string;
  marketplace?: MarketplaceMetadata | null;
  gcsPath: string;
  status: CustomBotStatus;
  createdAt: string;
  updatedAt: string;
}

export interface UserBotSummary {
  id: string;
  customBotName: string;
  acquiredAt: string;
  customBot?: CustomBotWithVersions;
}

export type CustomBotStatus =
  | 'approved'
  | 'declined'
  | 'awaiting_human_review'
  | 'pending_review'
  | 'published';

interface MarketplaceMetadata {
  isPublished?: boolean;
  [key: string]: any;
}

// Remove Firestore Timestamp interface - now using standard ISO strings

interface UseUserBotsReturn {
  userBots: UserBotSummary[];
  loading: boolean;
  error: string | null;
  refetch: () => void;
  isInstalled: (botName: string) => boolean;
}

export function useUserBots(): UseUserBotsReturn {
  const [userBots, setUserBots] = useState<UserBotSummary[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const { user } = useAuth();

  const fetchUserBots = useCallback(async () => {
    if (!user) {
      setUserBots([]);
      setLoading(false);
      return;
    }

    try {
      setLoading(true);
      setError(null);

      const response = await authFetch('/api/user-bots');

      if (!response.ok) {
        throw new Error('Failed to fetch user bots');
      }

      const data = await response.json();
      if (data.success) {
        setUserBots(data.data || []);
      } else {
        throw new Error(data.message || 'Failed to fetch user bots');
      }
    } catch (err: any) {
      setError(err.message);
      setUserBots([]);
    } finally {
      setLoading(false);
    }
  }, [user]);

  useEffect(() => {
    fetchUserBots();
  }, [fetchUserBots]);

  const isInstalled = (botName: string): boolean => {
    return userBots.some((bot) => bot.customBotName === botName);
  };

  const refetch = () => {
    fetchUserBots();
  };

  return {
    userBots,
    loading,
    error,
    refetch,
    isInstalled,
  };
}
