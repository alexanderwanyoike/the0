"use client";

import { useEffect, useState } from "react";
import DashboardLayout from "@/components/layouts/dashboard-layout";
import { ChatPanel } from "@/components/ai-agent/chat/ChatPanel";
import { ChatList } from "@/components/ai-agent/chat/ChatList";
import { CollapsibleArtifactsPanel } from "@/components/ai-agent/layout/CollapsibleArtifactsPanel";
import { ApiKeySetup } from "@/components/ai-agent/setup/ApiKeySetup";
import { useArtifactsStore } from "@/stores/ai-agent/artifactsStore";
import { useArtifacts } from "@/hooks/ai-agent/useArtifacts";
import { useChatSessions } from "@/hooks/ai-agent/useChatSessions";
import { apiService } from "@/lib/ai-agent/api";
import { withAuth } from "@/components/auth/with-auth";

function AIAgentPage() {
  const { forceShow } = useArtifactsStore();
  const { files } = useArtifacts();
  const { switchToSession, startNewChat, loadSessionsAndRestore } =
    useChatSessions();
  const shouldShowArtifacts = forceShow && files.length > 0;

  const [, setHasApiKey] = useState<boolean | null>(null);
  const [, setHasTavilyKey] = useState<boolean | null>(null);
  const [showApiKeySetup, setShowApiKeySetup] = useState(false);

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

  // Check API key status and load most recent session on startup
  useEffect(() => {
    const initializeApp = async () => {
      try {
        // Check Google AI key (REQUIRED)
        const status = await apiService.checkApiKeyStatus();
        setHasApiKey(status.has_api_key);
        if (!status.has_api_key) {
          setShowApiKeySetup(true);
          return; // Don't proceed without Google AI key
        }

        // Check Tavily key (OPTIONAL - don't block on this)
        try {
          const tavilyStatus = await apiService.checkTavilyApiKeyStatus();
          setHasTavilyKey(tavilyStatus.has_api_key);
        } catch (tavilyErr) {
          console.error("Failed to check Tavily status:", tavilyErr);
          // Continue anyway, Tavily is optional
          setHasTavilyKey(false);
        }

        // Load sessions and restore most recent session for the main page
        await loadSessionsAndRestore();
      } catch (error) {
        console.error("Failed to check API key status:", error);
        // Show setup anyway if we can't check
        setShowApiKeySetup(true);
      }
    };

    initializeApp();
  }, []); // Remove loadSessionsAndRestore from dependencies to prevent infinite loops

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
export default withAuth(AIAgentPage);
