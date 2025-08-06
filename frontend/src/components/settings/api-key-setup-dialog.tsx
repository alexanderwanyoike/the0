"use client";
import React, { useState } from "react";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent } from "@/components/ui/card";
import {
  Copy,
  Key,
  Calendar,
  Activity,
  Code,
  Terminal,
  FileCode,
} from "lucide-react";
import { ApiKey } from "@/lib/api/api-key-service";
import { useToast } from "@/hooks/use-toast";
import { formatDistanceToNow, format } from "date-fns";
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert";

interface ApiKeySetupDialogProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  apiKey: ApiKey;
}

export function ApiKeySetupDialog({
  open,
  onOpenChange,
  apiKey,
}: ApiKeySetupDialogProps) {
  const { toast } = useToast();
  const [copiedField, setCopiedField] = useState<string | null>(null);

  const copyToClipboard = (text: string, field: string) => {
    navigator.clipboard.writeText(text);
    setCopiedField(field);
    setTimeout(() => setCopiedField(null), 2000);
    toast({
      title: "Copied",
      description: `${field} copied to clipboard.`,
    });
  };

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="sm:max-w-4xl">
        <DialogHeader>
          <DialogTitle className="flex items-center gap-2">
            <Code className="h-5 w-5" />
            API Key Setup Instructions
          </DialogTitle>
          <DialogDescription>
            Learn how to use your API key with the0&#39;s services and CLI
            tools.
          </DialogDescription>
        </DialogHeader>

        <div className="space-y-6">
          {/* Key Information */}
          <div className="space-y-4">
            <div className="flex items-center justify-between">
              <div>
                <h3 className="font-medium">{apiKey.name}</h3>
                <p className="text-sm text-muted-foreground">
                  <span className="font-mono">{apiKey.key}</span>
                </p>
              </div>
              <Badge variant={apiKey.isActive ? "default" : "secondary"}>
                {apiKey.isActive ? "Active" : "Inactive"}
              </Badge>
            </div>

            <div className="grid grid-cols-1 sm:grid-cols-2 gap-4 text-sm">
              <div className="flex items-center gap-2">
                <Calendar className="h-4 w-4 text-muted-foreground" />
                <div>
                  <div className="font-medium">Created</div>
                  <div className="text-muted-foreground">
                    {format(new Date(apiKey.createdAt), "MMM d, yyyy")}
                    <br />
                    {formatDistanceToNow(new Date(apiKey.createdAt), {
                      addSuffix: true,
                    })}
                  </div>
                </div>
              </div>

              {apiKey.lastUsedAt && (
                <div className="flex items-center gap-2">
                  <Activity className="h-4 w-4 text-muted-foreground" />
                  <div>
                    <div className="font-medium">Last Used</div>
                    <div className="text-muted-foreground">
                      {format(new Date(apiKey.lastUsedAt), "MMM d, yyyy")}
                      <br />
                      {formatDistanceToNow(new Date(apiKey.lastUsedAt), {
                        addSuffix: true,
                      })}
                    </div>
                  </div>
                </div>
              )}
            </div>
          </div>

          {/* CLI Setup */}
          <div className="space-y-4">
            <div className="flex items-center gap-2">
              <Terminal className="h-4 w-4" />
              <h3 className="font-medium">CLI Setup</h3>
            </div>

            <Card>
              <CardContent className="p-4">
                <div className="flex items-center justify-between mb-2">
                  <h4 className="text-sm font-medium">Environment Variable</h4>
                  <Button
                    variant="ghost"
                    size="sm"
                    onClick={() =>
                      copyToClipboard(
                        `export THE0_API_KEY="your_api_key_here"`,
                        "Environment variable",
                      )
                    }
                    className="h-6 px-2"
                  >
                    <Copy className="h-3 w-3" />
                    {copiedField === "Environment variable"
                      ? "Copied!"
                      : "Copy"}
                  </Button>
                </div>
                <pre className="text-xs bg-muted p-2 rounded overflow-x-auto">
                  {`# Set your API key as environment variable
export THE0_API_KEY="your_api_key_here"
`}
                </pre>
              </CardContent>
            </Card>
          </div>

          {/* Security Notes */}
          <div className="space-y-4">
            <div className="flex items-center gap-2">
              <Key className="h-4 w-4" />
              <h3 className="font-medium">Security Best Practices</h3>
            </div>

            <Alert variant="destructive">
              <AlertTitle>Security Best Practices</AlertTitle>
              <AlertDescription className="text-sm space-y-2">
                <p>
                  • <strong>Keep your API key secure:</strong> Never share it
                  publicly or commit it to version control
                </p>
                <p>
                  • <strong>Use environment variables:</strong> Store keys in
                  environment variables, not in your code
                </p>
                <p>
                  • <strong>Rotate regularly:</strong> Create new keys
                  periodically and revoke old ones
                </p>
                <p>
                  • <strong>Revoke immediately:</strong> If compromised, revoke
                  the key and create a new one
                </p>
              </AlertDescription>
            </Alert>
          </div>
        </div>

        <div className="flex justify-end pt-4">
          <Button onClick={() => onOpenChange(false)}>Close</Button>
        </div>
      </DialogContent>
    </Dialog>
  );
}
