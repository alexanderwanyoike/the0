/**
 * Generic SSE Client Hook
 * 
 * Provides a reusable React hook for SSE connections with:
 * - Automatic reconnection logic
 * - Authentication handling
 * - Cleanup on component unmount
 * - Consistent loading/error state pattern
 * 
 * This hook follows the same patterns as existing hooks in the codebase
 * and can be used as a foundation for specific SSE implementations.
 */

'use client';

import { useCallback, useEffect, useRef, useState } from 'react';
import { SSEClient, SSEMessage, SSEConnectionState } from '@/lib/sse/sse-client';
import { createAuthenticatedSSEUrl, handleSSEAuthError, validateSSEAuth } from '@/lib/sse/sse-auth';
import { useAuth } from '@/contexts/auth-context';

export interface UseSSEClientOptions<T> {
  onMessage: (data: T) => void;
  onError?: (error: Event) => void;
  onConnected?: () => void;
  onDisconnected?: () => void;
  maxReconnectAttempts?: number;
  reconnectDelay?: number;
  enabled?: boolean;
  autoConnect?: boolean;
}

export interface SSEClientState {
  connected: boolean;
  loading: boolean;
  error: string | null;
  retryCount: number;
  lastUpdate: Date | null;
}

export function useSSEClient<T>(
  endpoint: string,
  options: UseSSEClientOptions<T>
) {
  const [state, setState] = useState<SSEClientState>({
    connected: false,
    loading: false,
    error: null,
    retryCount: 0,
    lastUpdate: null,
  });

  const sseClient = useRef<SSEClient>(new SSEClient());
  const disconnectRef = useRef<(() => void) | null>(null);
  const { user } = useAuth();

  const {
    onMessage,
    onError,
    onConnected,
    onDisconnected,
    maxReconnectAttempts = 5,
    reconnectDelay = 3000,
    enabled = true,
    autoConnect = true,
  } = options;

  // Handle SSE messages
  const handleMessage = useCallback((data: T) => {
    setState(prev => ({
      ...prev,
      lastUpdate: new Date(),
      error: null,
    }));
    onMessage(data);
  }, [onMessage]);

  // Handle SSE connection open
  const handleOpen = useCallback(() => {
    setState(prev => ({
      ...prev,
      connected: true,
      loading: false,
      error: null,
      retryCount: 0,
    }));
    onConnected?.();
  }, [onConnected]);

  // Handle SSE connection errors
  const handleError = useCallback((error: Event) => {
    setState(prev => ({
      ...prev,
      connected: false,
      loading: false,
      error: 'Connection error occurred',
      retryCount: prev.retryCount + 1,
    }));

    // Handle authentication errors
    handleSSEAuthError(error);
    
    onError?.(error);
  }, [onError]);

  // Handle SSE connection close
  const handleClose = useCallback(() => {
    setState(prev => ({
      ...prev,
      connected: false,
      loading: false,
    }));
    onDisconnected?.();
  }, [onDisconnected]);

  // Connect to SSE endpoint
  const connect = useCallback(async () => {
    if (!user || !enabled) {
      setState(prev => ({
        ...prev,
        loading: false,
        error: user ? null : 'Authentication required',
      }));
      return;
    }

    setState(prev => ({
      ...prev,
      loading: true,
      error: null,
    }));

    // Validate authentication
    const authResult = validateSSEAuth();
    if (!authResult.success) {
      setState(prev => ({
        ...prev,
        loading: false,
        error: authResult.error,
      }));
      return;
    }

    // Create authenticated URL
    const urlResult = createAuthenticatedSSEUrl(endpoint);
    if (!urlResult.success) {
      setState(prev => ({
        ...prev,
        loading: false,
        error: urlResult.error,
      }));
      return;
    }

    try {
      // Clean up any existing connection
      if (disconnectRef.current) {
        disconnectRef.current();
        disconnectRef.current = null;
      }

      // Establish new connection
      const disconnect = sseClient.current.connect<T>(
        urlResult.data.url,
        handleMessage,
        {
          onOpen: handleOpen,
          onError: handleError,
          onClose: handleClose,
          maxReconnectAttempts,
          reconnectDelay,
        }
      );

      disconnectRef.current = disconnect;
    } catch (error) {
      setState(prev => ({
        ...prev,
        loading: false,
        error: `Failed to establish SSE connection: ${error instanceof Error ? error.message : 'Unknown error'}`,
      }));
    }
  }, [user, enabled, endpoint, handleMessage, handleOpen, handleError, handleClose, maxReconnectAttempts, reconnectDelay]);

  // Disconnect from SSE endpoint
  const disconnect = useCallback(() => {
    if (disconnectRef.current) {
      disconnectRef.current();
      disconnectRef.current = null;
    }
    
    setState(prev => ({
      ...prev,
      connected: false,
      loading: false,
      error: null,
    }));
  }, []);

  // Reconnect to SSE endpoint
  const reconnect = useCallback(() => {
    disconnect();
    setTimeout(() => {
      connect();
    }, 100); // Small delay to ensure cleanup is complete
  }, [disconnect, connect]);

  // Auto-connect on mount and when dependencies change
  useEffect(() => {
    if (autoConnect && enabled && user) {
      connect();
    }

    // Cleanup on unmount
    return () => {
      if (disconnectRef.current) {
        disconnectRef.current();
        disconnectRef.current = null;
      }
    };
  }, [autoConnect, enabled, user, connect]);

  // Get current connection state from SSE client
  const getConnectionState = useCallback((): SSEConnectionState => {
    const urlResult = createAuthenticatedSSEUrl(endpoint);
    if (!urlResult.success) {
      return {
        connected: false,
        lastUpdate: null,
        error: urlResult.error,
        retryCount: 0,
      };
    }

    return sseClient.current.getConnectionState(urlResult.data.url);
  }, [endpoint]);

  return {
    // Connection state
    connected: state.connected,
    loading: state.loading,
    error: state.error,
    retryCount: state.retryCount,
    lastUpdate: state.lastUpdate,
    
    // Connection methods
    connect,
    disconnect,
    reconnect,
    
    // Utility methods
    getConnectionState,
    isEnabled: enabled && !!user,
  };
}

/**
 * Simplified SSE hook for basic use cases
 * 
 * Provides a simpler interface for components that just need
 * basic SSE functionality without advanced options.
 */
export function useSSE<T>(
  endpoint: string,
  onMessage: (data: T) => void,
  enabled: boolean = true
) {
  return useSSEClient<T>(endpoint, {
    onMessage,
    enabled,
    autoConnect: true,
  });
}

/**
 * SSE hook with manual connection control
 * 
 * Provides manual control over when to connect/disconnect,
 * useful for components that need precise control over SSE lifecycle.
 */
export function useManualSSE<T>(
  endpoint: string,
  onMessage: (data: T) => void
) {
  return useSSEClient<T>(endpoint, {
    onMessage,
    enabled: true,
    autoConnect: false,
  });
}