import { ChatRequest, ChatResponse, StreamChunk } from "@/types";

// Use Next.js API routes for proxying to the AI agent service
const API_BASE_URL = "/api/ai-agent";

console.log("API_BASE_URL:", API_BASE_URL);

// Environment detection for pywebview compatibility
const isRunningInPywebview = (): boolean => {
  const checks = {
    userAgent: navigator.userAgent.includes("pywebview"),
    pywebviewProperty: !!(window as any).pywebview,
    noHistory: !window.history?.pushState && !window.opener,
    chromeWithoutExtensions:
      navigator.userAgent.includes("Chrome") &&
      !navigator.userAgent.includes("Edg") &&
      !(window as any).chrome,
  };

  console.log("Environment detection checks:", checks);
  console.log("User agent:", navigator.userAgent);

  // Check for pywebview-specific indicators
  const result = !!(
    checks.userAgent ||
    checks.pywebviewProperty ||
    checks.noHistory ||
    checks.chromeWithoutExtensions
  );

  console.log("isRunningInPywebview result:", result);
  return result;
};

class ApiService {
  public readonly baseURL = API_BASE_URL;

  private async request<T>(
    endpoint: string,
    options: RequestInit = {},
  ): Promise<T> {
    const url = `${API_BASE_URL}${endpoint}`;

    try {
      const response = await fetch(url, {
        headers: {
          "Content-Type": "application/json",
          ...options.headers,
        },
        ...options,
      });

      if (!response.ok) {
        const errorData = await response.json().catch(() => ({}));
        const error = new Error(
          errorData.detail ||
            errorData.message ||
            `HTTP ${response.status}: ${response.statusText}`,
        );
        (error as any).status = response.status;
        throw error;
      }

      return await response.json();
    } catch (error) {
      if (error instanceof Error) {
        throw error;
      }

      throw new Error("An unknown error occurred");
    }
  }

  sendMessage = async (request: ChatRequest): Promise<ChatResponse> => {
    return this.request<ChatResponse>("/chat", {
      method: "POST",
      body: JSON.stringify(request),
    });
  };

  sendMessageStream = async (
    request: ChatRequest,
    onChunk: (chunk: StreamChunk) => void,
  ): Promise<void> => {
    console.log(
      "sendMessageStream: Environment check - isRunningInPywebview:",
      isRunningInPywebview(),
    );

    // Use improved streaming approach that handles pywebview compatibility
    return this.sendMessageStreamWithRetry(request, onChunk);
  };

  private sendMessageStreamWithRetry = async (
    request: ChatRequest,
    onChunk: (chunk: StreamChunk) => void,
  ): Promise<void> => {
    const url = `${API_BASE_URL}/chat/stream`;

    try {
      const response = await fetch(url, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          Accept: "text/event-stream",
          "Cache-Control": "no-cache",
        },
        body: JSON.stringify(request),
      });

      if (!response.ok) {
        const errorData = await response.json().catch(() => ({}));
        throw new Error(
          errorData.detail ||
            errorData.message ||
            `HTTP ${response.status}: ${response.statusText}`,
        );
      }

      // For pywebview, try to read the entire response and then simulate streaming
      if (isRunningInPywebview()) {
        const text = await response.text();
        this.simulateStreamingFromText(text, onChunk);
        return;
      }

      // Fallback to original streaming approach for development
      const reader = response.body?.getReader();
      if (!reader) {
        throw new Error("No response body");
      }

      const decoder = new TextDecoder();
      let buffer = "";

      try {
        while (true) {
          const { done, value } = await reader.read();

          if (done) break;

          buffer += decoder.decode(value, { stream: true });
          const lines = buffer.split("\n");

          buffer = lines.pop() || "";

          for (const line of lines) {
            const trimmed = line.trim();
            if (trimmed === "" || !trimmed.startsWith("data: ")) continue;

            const data = trimmed.slice(6);
            if (data === "[DONE]") return;

            try {
              const chunk: StreamChunk = JSON.parse(data);
              onChunk(chunk);
            } catch (error) {
              console.error("Failed to parse SSE chunk:", error, data);
            }
          }
        }
      } finally {
        reader.releaseLock();
      }
    } catch (error) {
      console.error("Streaming error, attempting fallback:", error);
      // If streaming fails entirely, fall back to non-streaming
      try {
        const response = await this.sendMessage(request);
        onChunk({
          type: "content",
          content: response.response,
        });
        if (response.artifacts && response.artifacts.length > 0) {
          onChunk({
            type: "artifacts",
            artifacts: response.artifacts,
          });
        }
        onChunk({
          type: "complete",
          session_id: response.session_id,
        });
      } catch (fallbackError) {
        throw fallbackError;
      }
    }
  };

  private simulateStreamingFromText = (
    text: string,
    onChunk: (chunk: StreamChunk) => void,
  ): void => {
    const lines = text.split("\n");
    let index = 0;

    const processNextLine = () => {
      if (index >= lines.length) return;

      const line = lines[index++].trim();
      if (line === "" || !line.startsWith("data: ")) {
        setTimeout(processNextLine, 10);
        return;
      }

      const data = line.slice(6);
      if (data === "[DONE]") return;

      try {
        const chunk: StreamChunk = JSON.parse(data);
        onChunk(chunk);

        // Add small delay to simulate streaming
        setTimeout(processNextLine, 50);
      } catch (error) {
        console.error("Failed to parse SSE chunk:", error, data);
        setTimeout(processNextLine, 10);
      }
    };

    processNextLine();
  };

  getArtifacts = async (): Promise<string[]> => {
    return this.request<string[]>("/artifacts");
  };

  getSessionArtifacts = async (sessionId: string): Promise<string[]> => {
    return this.request<string[]>(`/artifacts?session_id=${sessionId}`);
  };

  getArtifact = async (
    filename: string,
    sessionId?: string,
  ): Promise<{ filename: string; content: string; version: number }> => {
    let url = `/artifacts/${encodeURIComponent(filename)}`;
    if (sessionId) {
      url += `?session_id=${sessionId}`;
    }

    const result = await this.request<{
      filename: string;
      content: string;
      version: number;
    }>(url);
    return result;
  };

  healthCheck = async (): Promise<{ status: string; message: string }> => {
    return this.request("/health");
  };

  // Chat session management endpoints
  getChatSessions = async (): Promise<any[]> => {
    return this.request<any[]>("/sessions");
  };

  getChatSession = async (sessionId: string): Promise<any> => {
    return this.request(`/sessions/${sessionId}`);
  };

  updateChatSessionTitle = async (
    sessionId: string,
    title: string,
  ): Promise<void> => {
    await this.request(`/sessions/${sessionId}`, {
      method: "PUT",
      body: JSON.stringify({ title }),
    });
  };

  deleteChatSession = async (sessionId: string): Promise<void> => {
    await this.request(`/sessions/${sessionId}`, {
      method: "DELETE",
    });
  };

  // API key management endpoints
  checkApiKeyStatus = async (): Promise<{ has_api_key: boolean }> => {
    return this.request<{ has_api_key: boolean }>(
      "/settings?endpoint=api-key/status",
    );
  };

  setApiKey = async (apiKey: string): Promise<{ message: string }> => {
    return this.request<{ message: string }>("/settings?endpoint=api-key", {
      method: "POST",
      body: JSON.stringify({ api_key: apiKey }),
    });
  };

  resetApiKey = async (): Promise<{ message: string }> => {
    return this.request<{ message: string }>("/settings?endpoint=api-key", {
      method: "DELETE",
    });
  };

  // Update management endpoints
  checkForUpdates = async (): Promise<{
    update_available: boolean;
    current_version: string;
    latest_version?: string;
    release_date?: string;
    download_url?: string;
    error?: string;
  }> => {
    return this.request("/updates/check");
  };

  installUpdate = async (): Promise<{
    success: boolean;
    message?: string;
    error?: string;
    install_path?: string;
  }> => {
    return this.request("/updates/install", {
      method: "POST",
    });
  };

  getVersion = async (): Promise<{
    version: string;
    build_date: string;
    commit_hash: string;
  }> => {
    return this.request("/version");
  };
}

class ApiError extends Error {
  public status?: number;

  constructor({ message, status }: { message: string; status?: number }) {
    super(message);
    this.name = "ApiError";
    this.status = status;
  }
}

export const apiService = new ApiService();
export { ApiError };
