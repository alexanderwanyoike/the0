import { useState } from "react";
import { MessageList } from "./MessageList";
import { ChatInput } from "./ChatInput";
import { useStreamingChat } from "@/hooks/ai-agent/useStreamingChat";
import { useSettingsStore } from "@/stores/ai-agent/settingsStore";
import { SettingsModal } from "@/components/ai-agent/settings/SettingsModal";
import { Button } from "@/components/ui/button";
import { Settings } from "lucide-react";

export function ChatPanel() {
  const { streamingEnabled } = useSettingsStore();
  const { messages, sendMessage, isStreaming } = useStreamingChat();
  const [showSettings, setShowSettings] = useState(false);

  return (
    <div className="h-full flex flex-col bg-background">
      <div className="border-b border-border p-4">
        <div className="flex items-center justify-between">
          <div>
            <h2 className="font-semibold text-lg">Chat with the0</h2>
            <p className="text-sm text-muted-foreground">
              Describe your trading strategy and I'll help you build it
            </p>
          </div>
          <Button
            variant="ghost"
            size="icon"
            onClick={() => setShowSettings(true)}
            title="Settings"
          >
            <Settings className="h-5 w-5" />
          </Button>
        </div>
      </div>

      <div className="flex-1 overflow-hidden">
        <MessageList messages={messages} />
      </div>

      <div className="border-t border-border p-4">
        <ChatInput
          onSendMessage={(message) => sendMessage(message, streamingEnabled)}
          disabled={isStreaming}
        />
      </div>

      <SettingsModal open={showSettings} onOpenChange={setShowSettings} />
    </div>
  );
}
