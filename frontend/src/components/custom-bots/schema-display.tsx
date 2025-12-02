import React, { useState } from "react";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import {
  Collapsible,
  CollapsibleContent,
  CollapsibleTrigger,
} from "@/components/ui/collapsible";
import { Copy, ChevronDown, ChevronUp, FileJson } from "lucide-react";
import { useToast } from "@/hooks/use-toast";
import { BotSchema } from "@/lib/api/api-client";

interface SchemaDisplayProps {
  schema: BotSchema | undefined;
  botName: string;
}

export const SchemaDisplay: React.FC<SchemaDisplayProps> = ({
  schema,
  botName,
}) => {
  const { toast } = useToast();
  const [isOpen, setIsOpen] = useState(true);

  const handleCopySchema = () => {
    if (!schema) return;
    navigator.clipboard.writeText(JSON.stringify(schema, null, 2));
    toast({
      title: "Schema Copied",
      description: "Bot schema copied to clipboard.",
    });
  };

  if (!schema) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2 text-lg">
            <FileJson className="h-5 w-5" />
            Bot Schema
          </CardTitle>
        </CardHeader>
        <CardContent>
          <p className="text-sm text-muted-foreground">
            No schema defined for this bot.
          </p>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <Collapsible open={isOpen} onOpenChange={setIsOpen}>
        <CardHeader className="pb-3">
          <div className="flex items-center justify-between">
            <CardTitle className="flex items-center gap-2 text-lg">
              <FileJson className="h-5 w-5" />
              Bot Schema
            </CardTitle>
            <div className="flex items-center gap-2">
              <Button
                variant="outline"
                size="sm"
                onClick={handleCopySchema}
                className="gap-2"
              >
                <Copy className="h-4 w-4" />
                Copy
              </Button>
              <CollapsibleTrigger asChild>
                <Button variant="ghost" size="sm">
                  {isOpen ? (
                    <ChevronUp className="h-4 w-4" />
                  ) : (
                    <ChevronDown className="h-4 w-4" />
                  )}
                </Button>
              </CollapsibleTrigger>
            </div>
          </div>
          <p className="text-sm text-muted-foreground">
            JSON Schema defining the configuration parameters for {botName}
          </p>
        </CardHeader>
        <CollapsibleContent>
          <CardContent className="pt-0">
            <div className="relative">
              <pre className="text-xs bg-muted/50 p-4 rounded-lg font-mono overflow-auto max-h-[400px] whitespace-pre-wrap">
                {JSON.stringify(schema, null, 2)}
              </pre>
            </div>
            <p className="text-xs text-muted-foreground mt-3">
              Use this schema to create your <code>config.json</code> file for
              CLI deployment.
            </p>
          </CardContent>
        </CollapsibleContent>
      </Collapsible>
    </Card>
  );
};
