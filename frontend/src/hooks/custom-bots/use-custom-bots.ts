import { useEffect, useState, useCallback } from 'react';
import { useAuth } from '@/contexts/auth-context';
import { CustomBotVersion, CustomBotWithVersions } from '@/types/custom-bots';
import { CustomBotService } from '@/lib/api/custom-bots.service';

export interface UseCustomBotsReturn {
  bots: CustomBotWithVersions[];
  loading: boolean;
  error: string | null;
  refetch: () => Promise<void>;
}

export const useCustomBots = (): UseCustomBotsReturn => {
  const [bots, setBots] = useState<CustomBotWithVersions[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const { user } = useAuth();

  const fetchCustomBots = useCallback(async () => {
    if (!user?.id) {
      setBots([]);
      setLoading(false);
      return;
    }

    try {
      setLoading(true);
      setError(null);

      const result = await CustomBotService.getCustomBots();
      
      if (result.success) {
        setBots(result.data);
      } else {
        setError(result.error.message);
        setBots([]);
        
        // Handle specific error cases
        if (result.error.statusCode === 401) {
          console.warn('User unauthorized for custom bots');
        } else if (result.error.statusCode === 403) {
          console.warn('User forbidden from accessing custom bots');
        }
      }
    } catch (err: any) {
      console.error('Error fetching custom bots:', err);
      console.error('Error stack:', err.stack);
      if (err.message && err.message.includes('forEach')) {
        console.error('❌ This is the forEach error we are looking for!');
        console.error('❌ Error details:', err);
      }
      setError(err.message || 'Failed to load custom bots');
      setBots([]);
    } finally {
      setLoading(false);
    }
  }, [user?.id]);

  useEffect(() => {
    fetchCustomBots();
  }, [fetchCustomBots]);

  return {
    bots,
    loading,
    error,
    refetch: fetchCustomBots, // Provide manual refresh capability
  };
};
