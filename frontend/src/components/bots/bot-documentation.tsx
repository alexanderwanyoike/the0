import React, { useState } from "react";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { Separator } from "@/components/ui/separator";
import {
  FileText,
  ChevronDown,
  ChevronUp,
  Copy,
  ExternalLink,
  Info,
  Settings,
  Zap,
  AlertCircle,
} from "lucide-react";
import { useToast } from "@/hooks/use-toast";
import { CustomBotConfig } from "@/types/custom-bots";

interface BotDocumentationProps {
  config: CustomBotConfig;
  collapsed?: boolean;
  showMetadata?: boolean;
}

const BotDocumentation: React.FC<BotDocumentationProps> = ({
  config,
  collapsed = false,
  showMetadata = true,
}) => {
  const [isCollapsed, setIsCollapsed] = useState(collapsed);
  const { toast } = useToast();

  const handleCopyReadme = () => {
    if (config.readme) {
      navigator.clipboard.writeText(config.readme);
      toast({
        title: "Copied",
        description: "README content copied to clipboard",
        duration: 2000,
      });
    }
  };

  const formatReadme = (readme: string) => {
    // Basic markdown-like formatting for display
    return readme
      .replace(/^# (.*$)/gm, '<h1 class="text-lg font-bold mt-4 mb-2">$1</h1>')
      .replace(
        /^## (.*$)/gm,
        '<h2 class="text-base font-semibold mt-3 mb-2">$1</h2>',
      )
      .replace(
        /^### (.*$)/gm,
        '<h3 class="text-sm font-medium mt-2 mb-1">$1</h3>',
      )
      .replace(/\*\*(.*?)\*\*/g, "<strong>$1</strong>")
      .replace(/\*(.*?)\*/g, "<em>$1</em>")
      .replace(
        /`(.*?)`/g,
        '<code class="px-1 py-0.5 bg-muted rounded text-xs font-mono">$1</code>',
      )
      .replace(/\n/g, "<br>");
  };

  const getBotTypeIcon = (type: string) => {
    switch (type) {
      case "scheduled":
        return <Settings className="h-4 w-4" />;
      case "realtime":
        return <Zap className="h-4 w-4" />;
      case "event":
        return <AlertCircle className="h-4 w-4" />;
      default:
        return <FileText className="h-4 w-4" />;
    }
  };

  const getBotTypeColor = (type: string) => {
    switch (type) {
      case "scheduled":
        return "bg-blue-50 text-blue-700 border-blue-200";
      case "realtime":
        return "bg-green-50 text-green-700 border-green-200";
      case "event":
        return "bg-orange-50 text-orange-700 border-orange-200";
      default:
        return "bg-gray-50 text-gray-700 border-gray-200";
    }
  };

  return (
    <Card>
      <CardHeader className="pb-3">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-2">
            <FileText className="h-5 w-5" />
            <CardTitle className="text-base">Documentation</CardTitle>
          </div>
          <div className="flex items-center gap-2">
            {config.readme && (
              <Button
                variant="ghost"
                size="sm"
                onClick={handleCopyReadme}
                className="h-8 px-2"
              >
                <Copy className="h-4 w-4" />
              </Button>
            )}
            <Button
              variant="ghost"
              size="sm"
              onClick={() => setIsCollapsed(!isCollapsed)}
              className="h-8 px-2"
            >
              {isCollapsed ? (
                <ChevronDown className="h-4 w-4" />
              ) : (
                <ChevronUp className="h-4 w-4" />
              )}
            </Button>
          </div>
        </div>
      </CardHeader>

      {!isCollapsed && (
        <CardContent className="space-y-4">
          {/* Bot Metadata */}
          {showMetadata && (
            <div className="space-y-3">
              <div className="grid grid-cols-1 sm:grid-cols-2 gap-3">
                {/* Bot Type */}
                <div className="flex items-center gap-2">
                  <span className="text-sm text-muted-foreground min-w-0">
                    Type:
                  </span>
                  <Badge
                    variant="outline"
                    className={`gap-1 ${getBotTypeColor(config.type)}`}
                  >
                    {getBotTypeIcon(config.type)}
                    {config.type}
                  </Badge>
                </div>

                {/* Runtime */}
                <div className="flex items-center gap-2">
                  <span className="text-sm text-muted-foreground min-w-0">
                    Runtime:
                  </span>
                  <Badge variant="outline" className="gap-1">
                    {config.runtime}
                  </Badge>
                </div>

                {/* Version */}
                <div className="flex items-center gap-2">
                  <span className="text-sm text-muted-foreground min-w-0">
                    Version:
                  </span>
                  <Badge variant="outline">v{config.version}</Badge>
                </div>

                {/* Author */}
                <div className="flex items-center gap-2">
                  <span className="text-sm text-muted-foreground min-w-0">
                    Author:
                  </span>
                  <span className="text-sm font-medium">{config.author}</span>
                </div>
              </div>

              {/* Description */}
              {config.description && (
                <div className="space-y-1">
                  <span className="text-sm text-muted-foreground">
                    Description:
                  </span>
                  <p className="text-sm text-foreground leading-relaxed">
                    {config.description}
                  </p>
                </div>
              )}

              {/* Categories/Tags */}
              {config.metadata?.categories &&
                config.metadata.categories.length > 0 && (
                  <div className="space-y-2">
                    <span className="text-sm text-muted-foreground">
                      Categories:
                    </span>
                    <div className="flex flex-wrap gap-1">
                      {config.metadata.categories.map((category: string) => (
                        <Badge
                          key={category}
                          variant="secondary"
                          className="text-xs"
                        >
                          {category}
                        </Badge>
                      ))}
                    </div>
                  </div>
                )}

              {/* Instruments */}
              {config.metadata?.instruments &&
                config.metadata.instruments.length > 0 && (
                  <div className="space-y-2">
                    <span className="text-sm text-muted-foreground">
                      Instruments:
                    </span>
                    <div className="flex flex-wrap gap-1">
                      {config.metadata.instruments.map((instrument: string) => (
                        <Badge
                          key={instrument}
                          variant="outline"
                          className="text-xs"
                        >
                          {instrument}
                        </Badge>
                      ))}
                    </div>
                  </div>
                )}

              <Separator />
            </div>
          )}

          {/* README Content */}
          {config.readme ? (
            <div className="space-y-3">
              <div className="flex items-center gap-2">
                <FileText className="h-4 w-4 text-muted-foreground" />
                <span className="text-sm font-medium">README</span>
              </div>

              <div className="prose prose-sm max-w-none">
                <div
                  className="text-sm leading-relaxed space-y-2 bg-muted/30 p-4 rounded-md border"
                  dangerouslySetInnerHTML={{
                    __html: formatReadme(config.readme),
                  }}
                />
              </div>
            </div>
          ) : (
            <div className="flex items-center gap-2 p-4 bg-muted/30 rounded-md border border-dashed">
              <Info className="h-4 w-4 text-muted-foreground flex-shrink-0" />
              <div className="text-sm text-muted-foreground">
                <p className="font-medium">No documentation available</p>
                <p className="text-xs mt-1">
                  The bot author hasn&apos;t provided setup instructions or
                  documentation.
                </p>
              </div>
            </div>
          )}

          {/* Configuration Schema Info */}
          {config.schema?.bot && (
            <div className="space-y-2 pt-2 border-t">
              <div className="flex items-center gap-2">
                <Settings className="h-4 w-4 text-muted-foreground" />
                <span className="text-sm font-medium">
                  Configuration Parameters
                </span>
              </div>
              <div className="text-xs text-muted-foreground bg-muted/30 p-3 rounded border">
                <p>
                  This bot requires{" "}
                  {Object.keys(config.schema.bot.properties || {}).length}{" "}
                  configuration parameter
                  {Object.keys(config.schema.bot.properties || {}).length !== 1
                    ? "s"
                    : ""}
                  .
                </p>
                {config.schema.bot.required &&
                  config.schema.bot.required.length > 0 && (
                    <p className="mt-1">
                      {config.schema.bot.required.length} parameter
                      {config.schema.bot.required.length !== 1
                        ? "s are"
                        : " is"}{" "}
                      required.
                    </p>
                  )}
              </div>
            </div>
          )}
        </CardContent>
      )}
    </Card>
  );
};

export default BotDocumentation;
