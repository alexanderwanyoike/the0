"use client";

import { useEffect } from "react";
import { useParams } from "next/navigation";
import DashboardLayout from "@/components/layouts/dashboard-layout";
import { ChatPanel } from "@/components/ai-agent/chat/ChatPanel";
import { ChatList } from "@/components/ai-agent/chat/ChatList";
import { CollapsibleArtifactsPanel } from "@/components/ai-agent/layout/CollapsibleArtifactsPanel";
import { ApiKeySetup } from "@/components/ai-agent/setup/ApiKeySetup";
import { useArtifactsStore } from "@/stores/ai-agent/artifactsStore";
import { useArtifacts } from "@/hooks/ai-agent/useArtifacts";
import { useChatSessions } from "@/hooks/ai-agent/useChatSessions";
import { useThemeStore } from "@/stores/ai-agent/themeStore";
import { apiService } from "@/lib/ai-agent/api";
import { withAuth } from "@/components/auth/with-auth";
import { useState } from "react";

function SessionPage() {
  const params = useParams();
  const sessionId = params.sessionId as string;

  const { forceShow } = useArtifactsStore();
  const { files } = useArtifacts();
  const { switchToSession, startNewChat, loadSession, currentSessionId } =
    useChatSessions();
  const { isDark } = useThemeStore();
  const shouldShowArtifacts = forceShow && files.length > 0;

  const [, setHasApiKey] = useState<boolean | null>(null);
  const [showApiKeySetup, setShowApiKeySetup] = useState(false);

  // Apply theme immediately on component load
  useEffect(() => {
    if (isDark) {
      document.documentElement.classList.add("dark");
    } else {
      document.documentElement.classList.remove("dark");
    }
  }, [isDark]);

  // Handle zoom in/out with +/- keys
  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.ctrlKey || event.metaKey) {
        if (event.key === "=" || event.key === "+") {
          event.preventDefault();
          document.body.style.zoom = String(
            Math.min(2, parseFloat(document.body.style.zoom || "1") + 0.1),
          );
        } else if (event.key === "-") {
          event.preventDefault();
          document.body.style.zoom = String(
            Math.max(0.5, parseFloat(document.body.style.zoom || "1") - 0.1),
          );
        } else if (event.key === "0") {
          event.preventDefault();
          document.body.style.zoom = "1";
        }
      }
    };

    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  }, []);

  // Check API key status on startup
  useEffect(() => {
    const checkApiKeyStatus = async () => {
      try {
        const status = await apiService.checkApiKeyStatus();
        setHasApiKey(status.has_api_key);
        if (!status.has_api_key) {
          setShowApiKeySetup(true);
        }
      } catch (error) {
        console.error("Failed to check API key status:", error);
        // Show setup anyway if we can't check
        setShowApiKeySetup(true);
      }
    };

    checkApiKeyStatus();
  }, []);

  // Load the specific session from URL parameter
  useEffect(() => {
    if (sessionId) {
      console.log(
        "Loading session from URL:",
        sessionId,
        "current:",
        currentSessionId,
      );
      loadSession(sessionId);
    }
  }, [sessionId, loadSession]);

  const handleApiKeySetupComplete = () => {
    setShowApiKeySetup(false);
    setHasApiKey(true);
  };

  return (
    <DashboardLayout>
      <div className="h-[calc(100vh-3rem)] flex overflow-hidden">
        {/* Chat List Sidebar */}
        <div className="w-80 border-r border-border">
          <ChatList
            onSessionSelect={switchToSession}
            onNewChat={startNewChat}
          />
        </div>

        {/* Chat Panel */}
        <div
          className={`border-r border-border ${shouldShowArtifacts ? "w-[40%]" : "flex-1"}`}
        >
          <ChatPanel />
        </div>

        {/* Artifacts Panel */}
        <CollapsibleArtifactsPanel />
      </div>

      {/* API Key Setup Modal */}
      <ApiKeySetup
        open={showApiKeySetup}
        onComplete={handleApiKeySetupComplete}
        allowSkip={true}
      />
    </DashboardLayout>
  );
}

// Export the component wrapped with authentication
export default withAuth(SessionPage);
