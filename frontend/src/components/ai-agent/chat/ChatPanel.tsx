import { MessageList } from "./MessageList";
import { ChatInput } from "./ChatInput";
import { useStreamingChat } from "@/hooks/ai-agent/useStreamingChat";
import { useSettingsStore } from "@/stores/ai-agent/settingsStore";

export function ChatPanel() {
  const { streamingEnabled } = useSettingsStore();
  const { messages, sendMessage, isStreaming } = useStreamingChat();

  return (
    <div className="h-full flex flex-col bg-background">
      <div className="border-b border-border p-4">
        <h2 className="font-semibold text-lg">Chat with the0</h2>
        <p className="text-sm text-muted-foreground">
          Describe your trading strategy and I'll help you build it
        </p>
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
    </div>
  );
}
