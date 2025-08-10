import { useState } from "react";
import { useChatStore } from "@/stores/ai-agent/chatStore";
import { apiService } from "@/lib/ai-agent/api";
import { chatService } from "@/lib/ai-agent/chatService";
import { Message, ChatRequest } from "@/types";
import { v4 as uuidv4 } from "uuid";

export const useChat = () => {
  const {
    messages,
    sessionId,
    isTyping,
    addMessage,
    setTyping,
    setSessionId,
    setCurrentSession,
    updateSessionTitle,
    setSessions,
    sessions,
  } = useChatStore();

  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const sendMessage = async (content: string) => {
    if (isLoading) return;

    setIsLoading(true);
    setError(null);
    setTyping(true);

    // Add user message
    const userMessage: Message = {
      id: uuidv4(),
      content,
      role: "user",
      timestamp: new Date(),
      isStreaming: false,
      isComplete: true,
    };
    addMessage(userMessage);

    try {
      const request: ChatRequest = {
        message: content,
        session_id: sessionId || undefined,
      };

      const response = await apiService.sendMessage(request);

      // Add assistant message
      const assistantMessage: Message = {
        id: uuidv4(),
        content: response.response,
        role: "assistant",
        timestamp: new Date(),
        artifacts: response.artifacts,
        isStreaming: false,
        isComplete: true,
      };
      addMessage(assistantMessage);

      // Update session ID if new
      if (response.session_id && !sessionId) {
        setSessionId(response.session_id);
      }
    } catch (err) {
      const errorMessage =
        err instanceof Error ? err.message : "Unknown error occurred";
      setError(errorMessage);

      // Add error message
      const errorAssistantMessage: Message = {
        id: uuidv4(),
        content: `Error: ${errorMessage}`,
        role: "assistant",
        timestamp: new Date(),
        isStreaming: false,
        isComplete: true,
      };
      addMessage(errorAssistantMessage);
    } finally {
      setIsLoading(false);
      setTyping(false);
    }
  };

  return {
    messages,
    sessionId,
    isTyping,
    isLoading,
    error,
    sendMessage,
    setCurrentSession,
    updateSessionTitle,
    setSessions,
    sessions,
  };
};
