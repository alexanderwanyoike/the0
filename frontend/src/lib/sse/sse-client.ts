/**
 * Server-Sent Events (SSE) Client
 *
 * Provides a reusable SSE connection management class with:
 * - Connection pooling and cleanup mechanisms
 * - Authentication token handling
 * - Automatic reconnection with backoff
 * - Consistent error handling patterns
 */

export interface SSEMessage<T = any> {
  type: "connection" | "update" | "error" | "keepalive";
  data?: T;
  timestamp: string;
  error?: string;
}

export interface SSEConnectionState {
  connected: boolean;
  lastUpdate: Date | null;
  error: string | null;
  retryCount: number;
}

export interface SSEOptions {
  onOpen?: () => void;
  onError?: (error: Event) => void;
  onClose?: () => void;
  maxReconnectAttempts?: number;
  reconnectDelay?: number;
  withCredentials?: boolean;
}

const DEFAULT_OPTIONS: Required<
  Omit<SSEOptions, "onOpen" | "onError" | "onClose">
> = {
  maxReconnectAttempts: 5,
  reconnectDelay: 3000,
  withCredentials: true,
};

export class SSEClient {
  private connections: Map<string, EventSource> = new Map();
  private reconnectAttempts: Map<string, number> = new Map();
  private reconnectTimeouts: Map<string, NodeJS.Timeout> = new Map();

  /**
   * Establish SSE connection with automatic reconnection and error handling
   */
  connect<T>(
    url: string,
    onMessage: (data: T) => void,
    options: SSEOptions = {},
  ): () => void {
    const mergedOptions = { ...DEFAULT_OPTIONS, ...options };

    // Clean up any existing connection to this URL
    this.disconnect(url);

    const connect = () => {
      try {
        const eventSource = new EventSource(url, {
          withCredentials: mergedOptions.withCredentials,
        });

        eventSource.onopen = () => {
          this.reconnectAttempts.set(url, 0);
          console.log(`SSE connected to ${url}`);
          options.onOpen?.();
        };

        eventSource.onmessage = (event) => {
          try {
            const message: SSEMessage<T> = JSON.parse(event.data);

            switch (message.type) {
              case "update":
                if (message.data) {
                  onMessage(message.data);
                }
                break;
              case "error":
                console.error(`SSE error for ${url}:`, message.error);
                options.onError?.(new Event("message-error"));
                break;
              case "keepalive":
                console.debug(`SSE keepalive for ${url}`);
                break;
              case "connection":
                console.log(`SSE connection confirmed for ${url}`);
                break;
              default:
                console.warn(`Unknown SSE message type: ${message.type}`);
            }
          } catch (error) {
            console.error(`SSE message parsing error for ${url}:`, error);
            options.onError?.(new Event("parse-error"));
          }
        };

        eventSource.onerror = (error) => {
          console.error(`SSE connection error for ${url}:`, error);
          const attempts = this.reconnectAttempts.get(url) || 0;

          // Close the current connection
          eventSource.close();
          this.connections.delete(url);

          // Attempt reconnection if under limit
          if (attempts < mergedOptions.maxReconnectAttempts) {
            this.reconnectAttempts.set(url, attempts + 1);
            const delay = mergedOptions.reconnectDelay * Math.pow(2, attempts); // Exponential backoff

            console.log(
              `SSE reconnecting to ${url} in ${delay}ms (attempt ${attempts + 1}/${mergedOptions.maxReconnectAttempts})`,
            );

            const timeout = setTimeout(() => {
              this.reconnectTimeouts.delete(url);
              connect();
            }, delay);

            this.reconnectTimeouts.set(url, timeout);
          } else {
            console.error(`SSE max reconnection attempts reached for ${url}`);
            this.reconnectAttempts.delete(url);
          }

          options.onError?.(error);
        };

        this.connections.set(url, eventSource);
      } catch (error) {
        console.error(`Failed to create SSE connection for ${url}:`, error);
        options.onError?.(new Event("connection-failed"));
      }
    };

    // Start initial connection
    connect();

    // Return cleanup function
    return () => {
      this.disconnect(url);
    };
  }

  /**
   * Disconnect from a specific SSE endpoint
   */
  disconnect(url: string): void {
    const connection = this.connections.get(url);
    if (connection) {
      connection.close();
      this.connections.delete(url);
    }

    const timeout = this.reconnectTimeouts.get(url);
    if (timeout) {
      clearTimeout(timeout);
      this.reconnectTimeouts.delete(url);
    }

    this.reconnectAttempts.delete(url);
  }

  /**
   * Disconnect from all SSE endpoints
   */
  disconnectAll(): void {
    const urls = Array.from(this.connections.keys());
    urls.forEach((url) => this.disconnect(url));
  }

  /**
   * Get connection state for a specific URL
   */
  getConnectionState(url: string): SSEConnectionState {
    const connection = this.connections.get(url);
    const isConnected = connection?.readyState === EventSource.OPEN;

    return {
      connected: isConnected,
      lastUpdate: null, // Could track this if needed
      error: null, // Could track last error if needed
      retryCount: this.reconnectAttempts.get(url) || 0,
    };
  }

  /**
   * Check if connected to a specific URL
   */
  isConnected(url: string): boolean {
    const connection = this.connections.get(url);
    return connection?.readyState === EventSource.OPEN;
  }

  /**
   * Get all active connection URLs
   */
  getActiveConnections(): string[] {
    return Array.from(this.connections.keys()).filter((url) =>
      this.isConnected(url),
    );
  }
}

// Export singleton instance for convenience
export const sseClient = new SSEClient();
