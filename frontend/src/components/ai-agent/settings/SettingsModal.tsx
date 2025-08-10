import React, { useState } from "react";
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Separator } from "@/components/ui/separator";
import { Switch } from "@/components/ui/switch";
import { Slider } from "@/components/ui/slider";
import { Key, Trash2, Save, ExternalLink, MessageSquare } from "lucide-react";
import { apiService } from "@/lib/ai-agent/api";
import { useSettingsStore } from "@/stores/ai-agent/settingsStore";

interface SettingsModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
}

export function SettingsModal({ open, onOpenChange }: SettingsModalProps) {
  const [apiKey, setApiKey] = useState("");
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState("");
  const [success, setSuccess] = useState("");

  const {
    streamingEnabled,
    typewriterEnabled,
    typewriterSpeed,
    setStreamingEnabled,
    setTypewriterEnabled,
    setTypewriterSpeed,
  } = useSettingsStore();

  const handleSaveApiKey = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!apiKey.trim()) {
      setError("Please enter an API key");
      return;
    }

    setIsLoading(true);
    setError("");
    setSuccess("");

    try {
      await apiService.setApiKey(apiKey.trim());
      setSuccess("API key saved successfully");
      setApiKey("");
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to save API key");
    } finally {
      setIsLoading(false);
    }
  };

  const handleResetApiKey = async () => {
    if (
      !confirm(
        "Are you sure you want to reset your API key? This will require you to enter it again.",
      )
    ) {
      return;
    }

    setIsLoading(true);
    setError("");
    setSuccess("");

    try {
      await apiService.resetApiKey();
      setSuccess("API key reset successfully");
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to reset API key");
    } finally {
      setIsLoading(false);
    }
  };

  const openGoogleAIStudio = () => {
    window.open("https://aistudio.google.com/apikey", "_blank");
  };

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="sm:max-w-[500px]">
        <DialogHeader>
          <DialogTitle>Settings</DialogTitle>
        </DialogHeader>

        <div className="space-y-6">
          {/* API Key Section */}
          <div className="space-y-4">
            <div className="flex items-center gap-2">
              <Key className="h-5 w-5" />
              <h3 className="text-lg font-medium">Google AI API Key</h3>
            </div>

            <p className="text-sm text-gray-600 dark:text-gray-400">
              Manage your Google AI API key for accessing Gemini models.
            </p>

            <Button
              type="button"
              variant="outline"
              onClick={openGoogleAIStudio}
              className="w-full"
            >
              <ExternalLink className="h-4 w-4 mr-2" />
              Get API Key from Google AI Studio
            </Button>

            <form onSubmit={handleSaveApiKey} className="space-y-4">
              <div className="space-y-2">
                <Label htmlFor="settingsApiKey">New API Key</Label>
                <Input
                  id="settingsApiKey"
                  type="password"
                  placeholder="AIza..."
                  value={apiKey}
                  onChange={(e) => setApiKey(e.target.value)}
                  className="font-mono"
                />
              </div>

              <div className="flex gap-2">
                <Button type="submit" disabled={isLoading} className="flex-1">
                  <Save className="h-4 w-4 mr-2" />
                  {isLoading ? "Saving..." : "Save API Key"}
                </Button>
                <Button
                  type="button"
                  variant="destructive"
                  onClick={handleResetApiKey}
                  disabled={isLoading}
                >
                  <Trash2 className="h-4 w-4 mr-2" />
                  Reset
                </Button>
              </div>
            </form>

            {error && (
              <div className="text-sm text-red-600 dark:text-red-400 p-3 bg-red-50 dark:bg-red-950 rounded">
                {error}
              </div>
            )}

            {success && (
              <div className="text-sm text-green-600 dark:text-green-400 p-3 bg-green-50 dark:bg-green-950 rounded">
                {success}
              </div>
            )}
          </div>

          <Separator />

          {/* Streaming Settings */}
          <div className="space-y-4">
            <div className="flex items-center gap-2">
              <MessageSquare className="h-5 w-5" />
              <h3 className="text-lg font-medium">Chat Settings</h3>
            </div>

            <div className="space-y-4">
              <div className="flex items-center justify-between">
                <div className="space-y-0.5">
                  <Label htmlFor="streaming">Enable Streaming</Label>
                  <p className="text-sm text-muted-foreground">
                    Show AI responses as they're generated in real-time
                  </p>
                </div>
                <Switch
                  id="streaming"
                  checked={streamingEnabled}
                  onCheckedChange={setStreamingEnabled}
                />
              </div>

              <div className="flex items-center justify-between">
                <div className="space-y-0.5">
                  <Label htmlFor="typewriter">Typewriter Effect</Label>
                  <p className="text-sm text-muted-foreground">
                    Add smooth typewriter animation to streaming responses
                  </p>
                </div>
                <Switch
                  id="typewriter"
                  checked={typewriterEnabled}
                  onCheckedChange={setTypewriterEnabled}
                  disabled={!streamingEnabled}
                />
              </div>

              {streamingEnabled && typewriterEnabled && (
                <div className="space-y-2">
                  <div className="flex items-center justify-between">
                    <Label htmlFor="speed">Typewriter Speed</Label>
                    <span className="text-sm text-muted-foreground">
                      {typewriterSpeed} chars/sec
                    </span>
                  </div>
                  <Slider
                    id="speed"
                    min={10}
                    max={200}
                    step={10}
                    value={[typewriterSpeed]}
                    onValueChange={(value) => setTypewriterSpeed(value[0])}
                    className="w-full"
                  />
                  <div className="flex justify-between text-xs text-muted-foreground">
                    <span>Slow</span>
                    <span>Fast</span>
                  </div>
                </div>
              )}
            </div>
          </div>

          <Separator />

          {/* About Section */}
          <div className="space-y-2">
            <h3 className="text-lg font-medium">About</h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">
              the0 AI Agent Workbench - Build trading bots with AI assistance
            </p>
          </div>
        </div>
      </DialogContent>
    </Dialog>
  );
}
