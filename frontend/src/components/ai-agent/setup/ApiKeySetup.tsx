import React, { useState } from "react";
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
  DialogDescription,
} from "@/components/ui/dialog";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { ExternalLink, Key, AlertCircle, Search } from "lucide-react";
import { apiService } from "@/lib/ai-agent/api";

interface ApiKeySetupProps {
  open: boolean;
  onComplete: () => void;
  allowSkip?: boolean;
}

export function ApiKeySetup({
  open,
  onComplete,
  allowSkip = false,
}: ApiKeySetupProps) {
  const [apiKey, setApiKey] = useState("");
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState("");
  const [tavilyApiKey, setTavilyApiKey] = useState("");
  const [tavilyError, setTavilyError] = useState("");

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    // STEP 1: Validate and save Google AI key (REQUIRED)
    if (!apiKey.trim()) {
      setError("Please enter a Google AI API key");
      return;
    }

    setIsLoading(true);
    setError("");
    setTavilyError("");

    try {
      // Save Google AI key (MUST succeed)
      await apiService.setApiKey(apiKey.trim());

      // STEP 2: Try to save Tavily key if provided (OPTIONAL)
      if (tavilyApiKey.trim()) {
        // Validate Tavily format
        if (!tavilyApiKey.trim().startsWith("tvly-")) {
          setTavilyError('Tavily API keys must start with "tvly-"');
          // Don't return, continue with completion
        } else {
          try {
            await apiService.setTavilyApiKey(tavilyApiKey.trim());
            // Success! But don't show message, just complete setup
          } catch (tavilyErr) {
            // Log error but don't block setup completion
            console.error("Failed to save Tavily key:", tavilyErr);
            setTavilyError(
              "Tavily key couldn't be saved. You can configure it later in settings.",
            );
          }
        }
      }

      // STEP 3: Complete setup (even if Tavily failed)
      onComplete();
    } catch (err) {
      // Only Google AI errors block completion
      setError(err instanceof Error ? err.message : "Failed to save API key");
    } finally {
      setIsLoading(false);
    }
  };

  const openGoogleAIStudio = () => {
    window.open("https://aistudio.google.com/apikey", "_blank");
  };

  return (
    <Dialog open={open} onOpenChange={() => {}}>
      <DialogContent
        className="sm:max-w-[500px]"
        onPointerDownOutside={(e) => e.preventDefault()}
      >
        <DialogHeader>
          <DialogTitle className="flex items-center gap-2">
            <Key className="h-5 w-5" />
            Setup Google AI API Key
          </DialogTitle>
          <DialogDescription>
            To use the0 AI Agent Workbench, you need to provide a Google AI API
            key. This allows the agent to access Gemini models for building
            trading bots.
          </DialogDescription>
        </DialogHeader>

        <div className="space-y-4">
          <div className="bg-blue-50 dark:bg-blue-950 p-4 rounded-lg">
            <div className="flex items-start gap-3">
              <AlertCircle className="h-5 w-5 text-blue-600 dark:text-blue-400 mt-0.5 flex-shrink-0" />
              <div className="space-y-2 text-sm">
                <p className="font-medium text-blue-900 dark:text-blue-100">
                  How to get your API key:
                </p>
                <ol className="list-decimal list-inside space-y-1 text-blue-800 dark:text-blue-200">
                  <li>Click the button below to visit Google AI Studio</li>
                  <li>Sign in with your Google account</li>
                  <li>Click "Create API Key" and copy it</li>
                  <li>Paste it in the field below</li>
                </ol>
              </div>
            </div>
          </div>

          <Button
            type="button"
            variant="outline"
            onClick={openGoogleAIStudio}
            className="w-full"
          >
            <ExternalLink className="h-4 w-4 mr-2" />
            Get API Key from Google AI Studio
          </Button>

          <form onSubmit={handleSubmit} className="space-y-4">
            <div className="space-y-2">
              <Label htmlFor="apiKey">Google AI API Key</Label>
              <Input
                id="apiKey"
                type="password"
                placeholder="AIza..."
                value={apiKey}
                onChange={(e) => setApiKey(e.target.value)}
                className="font-mono"
              />
            </div>

            {error && (
              <div className="text-sm text-red-600 dark:text-red-400">
                {error}
              </div>
            )}

            {/* Optional Separator */}
            <div className="relative my-6">
              <div className="absolute inset-0 flex items-center">
                <span className="w-full border-t" />
              </div>
              <div className="relative flex justify-center text-xs uppercase">
                <span className="bg-background px-2 text-muted-foreground">
                  Optional
                </span>
              </div>
            </div>

            {/* Tavily Section */}
            <div className="space-y-4">
              <div className="bg-blue-50 dark:bg-blue-950 p-4 rounded-lg">
                <div className="flex items-start gap-3">
                  <Search className="h-5 w-5 text-blue-600 dark:text-blue-400 mt-0.5 flex-shrink-0" />
                  <div className="space-y-2 text-sm">
                    <p className="font-medium text-blue-900 dark:text-blue-100">
                      Tavily Web Search (Optional)
                    </p>
                    <p className="text-blue-800 dark:text-blue-200">
                      Enable web search capabilities for researching latest
                      documentation, trading APIs, and best practices. You can
                      skip this and configure it later in settings.
                    </p>
                  </div>
                </div>
              </div>

              <Button
                type="button"
                variant="outline"
                onClick={() => window.open("https://tavily.com", "_blank")}
                className="w-full"
              >
                <ExternalLink className="h-4 w-4 mr-2" />
                Get API Key from Tavily
              </Button>

              <div className="space-y-2">
                <Label htmlFor="tavilyApiKey">Tavily API Key (Optional)</Label>
                <Input
                  id="tavilyApiKey"
                  type="password"
                  placeholder="tvly-... (leave empty to skip)"
                  value={tavilyApiKey}
                  onChange={(e) => setTavilyApiKey(e.target.value)}
                  className="font-mono"
                />
              </div>

              {tavilyError && (
                <div className="text-sm text-red-600 dark:text-red-400">
                  {tavilyError}
                </div>
              )}
            </div>

            <div className="flex gap-2">
              <Button type="submit" disabled={isLoading} className="flex-1">
                {isLoading ? "Saving..." : "Save API Key"}
              </Button>
              {allowSkip && (
                <Button type="button" variant="outline" onClick={onComplete}>
                  Skip for now
                </Button>
              )}
            </div>
          </form>

          <div className="text-xs text-gray-600 dark:text-gray-400">
            <p>
              🔒 Your API keys are stored securely and never shared with
              external services.
            </p>
          </div>
        </div>
      </DialogContent>
    </Dialog>
  );
}
