import { Bot } from "lucide-react";
import React from "react";
import Link from "next/link";

export const EmptyState = () => (
  <div className="flex flex-col items-center justify-center py-16 px-4">
    <div className="w-20 h-20 bg-muted rounded-full flex items-center justify-center mb-6">
      <Bot className="w-10 h-10 text-muted-foreground" />
    </div>
    <h3 className="text-xl font-semibold mb-2">No Custom Bots</h3>
    <p className="text-muted-foreground text-center max-w-md mb-6">
      You haven&#39;t uploaded any custom trading bots yet. Use the CLI to
      upload your first bot and get started with automated trading.
    </p>
    <div className="bg-muted/50 border rounded-lg p-4 max-w-md">
      <h4 className="font-medium mb-2">Get Started</h4>
      <code className="text-sm bg-background px-2 py-1 rounded border">
        the0 custom-bot deploy
      </code>
      <p className="text-xs text-muted-foreground mt-2">
        Deploy your custom bot via CLI and track its approval status here.{" "}
        <Link
          href="/docs/the0-CLI/custom-bot-commands"
          className="text-primary hover:underline"
        >
          View docs
        </Link>
      </p>
    </div>
  </div>
);
