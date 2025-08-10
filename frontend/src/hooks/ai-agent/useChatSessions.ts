import { useEffect, useCallback } from "react";
import { useRouter } from "next/navigation";
import { useChatStore } from "@/stores/ai-agent/chatStore";
import { chatService } from "@/lib/ai-agent/chatService";

export function useChatSessions() {
  const router = useRouter();
  const {
    sessions,
    currentSessionId,
    messages,
    setSessions,
    setCurrentSession,
    loadSessionMessages,
    createNewSession,
    setSessionId,
  } = useChatStore();

  // Make all functions stable using useCallback
  const loadSessions = useCallback(async () => {
    try {
      const sessionsData = await chatService.getSessions();
      setSessions(sessionsData);
    } catch (error) {
      console.error("Failed to load sessions:", error);
    }
  }, []);

  const loadSession = useCallback(async (sessionId: string) => {
    try {
      console.log("Loading session:", sessionId);
      const sessionData = await chatService.getSession(sessionId);
      console.log("Session data received:", sessionData);
      const messages = chatService.convertToMessages(sessionData.messages);
      console.log("Converted messages:", messages);

      setCurrentSession(sessionId);
      setSessionId(sessionId);
      loadSessionMessages(messages);
      console.log("Session loaded successfully");
    } catch (error) {
      console.error("Failed to load session:", sessionId, error);

      // Don't create new session if it's a server error (500)
      // This prevents infinite loops when the API is having issues
      if (error instanceof Error && error.message.includes("500")) {
        console.error("Server error loading session, not creating new session");
        return;
      }

      // Only create new session for client errors (404, etc)
      console.log("Creating new session due to load failure");
      createNewSession();
    }
  }, []);

  const loadSessionsAndRestore = useCallback(async () => {
    try {
      // Load all sessions
      await loadSessions();

      // Load the most recent session automatically
      const mostRecentSession = await chatService.getMostRecentSession();
      if (mostRecentSession) {
        await loadSession(mostRecentSession.id);
      }
    } catch (error) {
      console.error("Failed to load sessions and restore:", error);
    }
  }, []);

  const startNewChat = useCallback(() => {
    createNewSession();
    // Navigate to main ai-agent page for new chats
    router.push("/ai-agent");
  }, []);

  const switchToSession = useCallback(async (sessionId: string) => {
    // Only navigate if we're not already on the session page
    const currentPath = window.location.pathname;
    const targetPath = `/ai-agent/session/${sessionId}`;

    if (currentPath !== targetPath) {
      console.log("Navigating to session:", sessionId);
      router.push(targetPath);
    } else {
      console.log("Already on session page, loading session data:", sessionId);
      // We're already on the session page, just load the session data
      await loadSession(sessionId);
    }
  }, []);

  // Function to refresh sessions from outside
  const refreshSessions = useCallback(async () => {
    await loadSessions();
  }, []);

  // Load sessions on mount (but don't auto-restore unless explicitly needed)
  useEffect(() => {
    loadSessions();
  }, [loadSessions]);

  return {
    sessions,
    currentSessionId,
    messages,
    loadSessions,
    loadSession,
    loadSessionsAndRestore,
    startNewChat,
    switchToSession,
    refreshSessions,
  };
}
