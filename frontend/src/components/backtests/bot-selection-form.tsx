"use client";

import React, { useState, useRef, useEffect } from "react";
import { Search, X, Bot, ChevronDown } from "lucide-react";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { useCombinedBotSearch } from "@/hooks/backtests/use-combined-bot-search";
import { cn } from "@/lib/utils";
import { BotSearchResult } from "@/types/backtest";
import { authFetch } from "@/lib/auth-fetch";

interface BotSelectionFormProps {
  selectedBot?: BotSearchResult | null;
  onBotSelect: (bot: BotSearchResult | null) => void;
  preSelectedBotName?: string;
  preSelectedBotVersion?: string;
  className?: string;
  selectedVersion?: string;
  onVersionSelect?: (version: string) => void;
}

export const BotSelectionForm: React.FC<BotSelectionFormProps> = ({
  selectedBot,
  onBotSelect,
  preSelectedBotName,
  preSelectedBotVersion,
  className,
  selectedVersion,
  onVersionSelect,
}) => {
  const [query, setQuery] = useState("");
  const [isOpen, setIsOpen] = useState(false);
  const [availableVersions, setAvailableVersions] = useState<string[]>([]);
  const [versionsLoading, setVersionsLoading] = useState(false);
  const { suggestions, search, loading, clearSuggestions } =
    useCombinedBotSearch();
  const inputRef = useRef<HTMLInputElement>(null);
  const containerRef = useRef<HTMLDivElement>(null);

  // Handle pre-selected bot name and version from URL params
  useEffect(() => {
    if (preSelectedBotName && preSelectedBotVersion && !selectedBot) {
      // Find the bot in suggestions by name
      const foundBot = suggestions.find(
        (bot) => bot.name === preSelectedBotName,
      );
      if (foundBot) {
        onBotSelect(foundBot);
        setQuery(foundBot.name);
        // Set the version from URL
        if (onVersionSelect) {
          onVersionSelect(preSelectedBotVersion);
        }
      } else if (preSelectedBotName) {
        // If not found in suggestions, create a bot result from URL params
        const botResult: BotSearchResult = {
          id: "", // We'll set this when available
          name: preSelectedBotName,
          resultType: "custom",
          approved: true,
        };
        onBotSelect(botResult);
        setQuery(preSelectedBotName);
        if (onVersionSelect) {
          onVersionSelect(preSelectedBotVersion);
        }
      }
    }
  }, [
    preSelectedBotName,
    preSelectedBotVersion,
    selectedBot,
    suggestions,
    onBotSelect,
    onVersionSelect,
  ]);

  // Fetch available versions when bot is selected
  useEffect(() => {
    if (selectedBot) {
      const fetchVersions = async () => {
        setVersionsLoading(true);
        try {
          const response = await authFetch(
            `/api/custom-bots/${encodeURIComponent(selectedBot.name)}/versions`,
          );
          if (response.ok) {
            const result = await response.json();
            if (result.success && result.data && Array.isArray(result.data)) {
              setAvailableVersions(result.data);
            } else {
              setAvailableVersions([]);
            }
          } else {
            setAvailableVersions([]);
          }
        } catch (error) {
          console.error("Error fetching versions:", error);
          setAvailableVersions([]);
        } finally {
          setVersionsLoading(false);
        }
      };

      fetchVersions();
    } else {
      setAvailableVersions([]);
    }
  }, [selectedBot]);

  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (
        containerRef.current &&
        !containerRef.current.contains(event.target as Node)
      ) {
        setIsOpen(false);
      }
    };

    document.addEventListener("mousedown", handleClickOutside);
    return () => document.removeEventListener("mousedown", handleClickOutside);
  }, []);

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const value = e.target.value;
    setQuery(value);

    if (value.length >= 2) {
      search(value);
      setIsOpen(true);
    } else {
      setIsOpen(false);
      clearSuggestions();
    }

    // Clear selection if user is typing
    if (selectedBot) {
      onBotSelect(null);
    }
  };

  const handleBotSelect = (bot: BotSearchResult) => {
    setQuery(bot.name);
    onBotSelect(bot);
    setIsOpen(false);
  };

  const handleClear = () => {
    setQuery("");
    onBotSelect(null);
    setIsOpen(false);
    clearSuggestions();
    inputRef.current?.focus();
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Escape") {
      setIsOpen(false);
    }
  };

  const getTypeColor = (type: string) => {
    switch (type) {
      case "marketplace":
        return "default";
      case "custom":
        return "secondary";
      case "user":
        return "outline";
      default:
        return "outline";
    }
  };

  const getTypeIcon = (type: string) => {
    switch (type) {
      case "marketplace":
        return "🏪";
      case "custom":
        return "🔧";
      case "user":
        return "👤";
      default:
        return "🤖";
    }
  };

  return (
    <div className={cn("space-y-4", className)}>
      {/* Bot Search Input */}
      <div>
        <label className="text-sm font-medium mb-2 block">Select Bot</label>
        <div ref={containerRef} className="relative w-full">
          <div className="relative">
            <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground" />
            <Input
              ref={inputRef}
              type="text"
              placeholder="Search for a bot to backtest..."
              value={query}
              onChange={handleInputChange}
              onKeyDown={handleKeyDown}
              onFocus={() => query.length >= 2 && setIsOpen(true)}
              className="pl-10 pr-10"
            />
            {query && (
              <Button
                variant="ghost"
                size="sm"
                onClick={handleClear}
                className="absolute right-2 top-1/2 transform -translate-y-1/2 h-6 w-6 p-0 hover:bg-muted"
              >
                <X className="h-3 w-3" />
              </Button>
            )}
          </div>

          {/* Suggestions Dropdown */}
          {isOpen && suggestions.length > 0 && (
            <div className="absolute top-full left-0 right-0 z-50 mt-1 bg-background border border-border rounded-md shadow-lg max-h-64 overflow-y-auto">
              {suggestions.map((bot, index) => (
                <button
                  key={`${bot.resultType}-${bot.id}-${index}`}
                  onClick={() => handleBotSelect(bot)}
                  className="w-full text-left px-4 py-3 hover:bg-muted transition-colors border-b last:border-b-0"
                >
                  <div className="flex items-center justify-between">
                    <div className="flex items-center gap-3">
                      <div className="flex items-center gap-2">
                        <Bot className="h-4 w-4 text-muted-foreground" />
                        <span className="font-medium">{bot.name}</span>
                      </div>
                      <div className="flex items-center gap-2">
                        <Badge
                          variant={getTypeColor(bot.resultType)}
                          className="text-xs"
                        >
                          {getTypeIcon(bot.resultType)} {bot.resultType}
                        </Badge>
                      </div>
                    </div>
                  </div>
                </button>
              ))}
              {loading && (
                <div className="px-4 py-3 text-sm text-muted-foreground">
                  Searching...
                </div>
              )}
            </div>
          )}

          {/* No results message */}
          {isOpen &&
            !loading &&
            query.length >= 2 &&
            suggestions.length === 0 && (
              <div className="absolute top-full left-0 right-0 z-50 mt-1 bg-background border border-border rounded-md shadow-lg p-4">
                <div className="text-center text-muted-foreground">
                  <Bot className="h-8 w-8 mx-auto mb-2 opacity-50" />
                  <p className="text-sm">
                    No bots found for &quot;{query}&quot;
                  </p>
                  <p className="text-xs mt-1">Try a different search term</p>
                </div>
              </div>
            )}
        </div>
      </div>

      {/* Selected Bot Display */}
      {selectedBot && (
        <div className="rounded-lg border bg-muted/50 p-4">
          <div className="flex items-center justify-between">
            <div className="flex items-center gap-3">
              <div className="p-2 rounded-lg bg-primary/10">
                <Bot className="h-5 w-5 text-primary" />
              </div>
              <div>
                <h3 className="font-medium">{selectedBot.name}</h3>
                <div className="flex items-center gap-2 mt-1">
                  <Badge
                    variant={getTypeColor(selectedBot.resultType)}
                    className="text-xs"
                  >
                    {getTypeIcon(selectedBot.resultType)}{" "}
                    {selectedBot.resultType}
                  </Badge>
                </div>
              </div>
            </div>
            <Button
              variant="ghost"
              size="sm"
              onClick={handleClear}
              className="text-muted-foreground hover:text-foreground"
            >
              <X className="h-4 w-4" />
            </Button>
          </div>
        </div>
      )}

      {/* Version Selector */}
      {selectedBot && (
        <div>
          <label className="text-sm font-medium mb-2 block">
            Select Version
          </label>
          {versionsLoading ? (
            <div className="flex items-center gap-2 text-sm text-muted-foreground">
              <div className="h-4 w-4 border-2 border-current border-t-transparent rounded-full animate-spin" />
              Loading versions...
            </div>
          ) : availableVersions.length > 0 ? (
            <Select value={selectedVersion} onValueChange={onVersionSelect}>
              <SelectTrigger>
                <SelectValue placeholder="Choose a version" />
              </SelectTrigger>
              <SelectContent>
                {availableVersions.map((version) => (
                  <SelectItem key={version} value={version}>
                    v{version}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          ) : (
            <div className="text-sm text-muted-foreground">
              No versions available for this bot
            </div>
          )}
        </div>
      )}
    </div>
  );
};
