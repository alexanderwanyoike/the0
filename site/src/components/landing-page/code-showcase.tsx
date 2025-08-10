"use client";
import { useState, useEffect } from "react";
import { useTheme } from "next-themes";
import { Prism as SyntaxHighlighter } from "react-syntax-highlighter";
import {
  oneDark,
  oneLight,
} from "react-syntax-highlighter/dist/esm/styles/prism";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import {
  Copy,
  Check,
  FileText,
  Settings,
  Package,
  Terminal,
  Code,
  Zap,
} from "lucide-react";

const codeFiles = [
  {
    id: "main",
    name: "main.py",
    icon: FileText,
    badge: "Python",
    language: "python",
    description: "Core bot logic with Alpaca integration",
    content: `from typing import Dict, Any
import logging
from datetime import datetime
from alpaca.trading.client import TradingClient
from alpaca.trading.requests import MarketOrderRequest
from alpaca.trading.enums import OrderSide, TimeInForce

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def main(id: str, config: Dict[str, Any]) -> Dict[str, Any]:
    """
    Main DCA bot execution function using Alpaca.
    
    Args:
        id: Unique bot instance ID
        config: Bot configuration from user
        
    Returns:
        Dict with execution status and results
    """
    logger.info(f"Starting DCA bot {id}")
    
    try:
        # Extract configuration
        api_key = config["api_key"]
        secret_key = config["secret_key"]
        paper = config.get("paper", True)
        symbol = config["symbol"]
        amount = config["amount"]
        
        # Initialize Alpaca trading client
        client = TradingClient(
            api_key=api_key,
            secret_key=secret_key,
            paper=paper
        )
        
        # Calculate and execute purchase
        order = MarketOrderRequest(
            symbol=symbol,
            notional=amount,
            side=OrderSide.BUY,
            time_in_force=TimeInForce.DAY
        )
        
        result = client.submit_order(order_data=order)
        
        return {
            "status": "success",
            "message": f"Purchased $\{amount\} of \{symbol\}",
            "order_id": str(result.id)
        }
        
    except Exception as e:
        logger.error(f"Bot execution failed: {e}")
        return {
            "status": "error",
            "message": str(e),
            "timestamp": datetime.now().isoformat()
        }`,
  },
  {
    id: "config",
    name: "bot-config.yaml",
    icon: Settings,
    badge: "YAML",
    language: "yaml",
    description: "Bot metadata and configuration",
    content: `name: killer-dca-bot
description: "A Dollar Cost Averaging bot that brings home the bacon 🥓"
version: 1.0.0
author: Jim Simons
type: scheduled
runtime: python3.11

entrypoints:
  bot: main.py
  backtest: backtest.py

schema:
  bot: bot-schema.json
  backtest: backtest-schema.json

readme: README.md

metadata:
  categories: [trading]
  instruments: [crypto, stocks]
  exchanges: [alpaca]
  tags: [dca, crypto, stocks]`,
  },
  {
    id: "schema",
    name: "bot-schema.json",
    icon: Settings,
    badge: "JSON Schema",
    language: "json",
    description: "Type-safe configuration validation",
    content: `{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "DCA Bot Configuration",
  "properties": {
    "api_key": {
      "type": "string",
      "description": "Your Alpaca API key"
    },
    "secret_key": {
      "type": "string",
      "description": "Your Alpaca secret key"
    },
    "paper": {
      "type": "boolean",
      "default": true,
      "description": "Whether to use paper trading"
    },
    "symbol": {
      "type": "string",
      "description": "Trading symbol (e.g., 'AAPL', 'BTC/USD')"
    },
    "asset_type": {
      "type": "string", 
      "description": "Type of asset to trade"
    },
    "amount": {
      "type": "number",
      "description": "Amount to invest"
    }
  },
  "required": [
    "api_key",
    "secret_key", 
    "paper",
    "symbol",
    "asset_type",
    "amount"
  ],
  "additionalProperties": false
}`,
  },
  {
    id: "requirements",
    name: "requirements.txt",
    icon: Package,
    badge: "Dependencies",
    language: "text",
    description: "Python package dependencies",
    content: `alpaca-py>=0.42.0`,
  },
];

const features = [
  {
    icon: Code,
    title: "Language Agnostic",
    description:
      "We will be language agnostic. Use Python, JavaScript, or any runtime with your favorite libraries and frameworks.",
  },
  {
    icon: Settings,
    title: "Type-Safe Configuration",
    description:
      "JSON Schema validation ensures your bot configuration is always correct.",
  },
  {
    icon: Zap,
    title: "Real-time Execution",
    description:
      "Deploy scheduled or continuous bots with live market data integration.",
  },
];

export function CodeShowcaseSection() {
  const [activeFile, setActiveFile] = useState("main");
  const [copiedFile, setCopiedFile] = useState<string | null>(null);
  const [mounted, setMounted] = useState(false);
  const { theme } = useTheme();

  useEffect(() => {
    setMounted(true);
  }, []);

  const copyToClipboard = async (content: string, fileId: string) => {
    try {
      await navigator.clipboard.writeText(content);
      setCopiedFile(fileId);
      setTimeout(() => setCopiedFile(null), 2000);
    } catch (err) {
      console.error("Failed to copy: ", err);
    }
  };

  const activeCodeFile = codeFiles.find((file) => file.id === activeFile);
  const syntaxTheme = mounted && theme === "dark" ? oneDark : oneLight;

  return (
    <section className="py-16 md:py-24 bg-muted/10">
      <div className="container max-w-7xl">
        <div className="text-center mb-16">
          <h2 className="text-3xl font-bold mb-4">
            Build Your First Trading Bot
          </h2>
          <p className="text-xl text-muted-foreground max-w-2xl mx-auto">
            A complete Dollar Cost Averaging bot with just a few files. We will
            be language agnostic - use any libraries you prefer.
          </p>
        </div>

        <div className="grid lg:grid-cols-12 gap-8 items-start">
          {/* File Browser */}
          <div className="lg:col-span-4">
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <Terminal className="h-5 w-5" />
                  Project Files
                </CardTitle>
              </CardHeader>
              <CardContent className="p-0">
                <div className="space-y-1">
                  {codeFiles.map((file) => {
                    const Icon = file.icon;
                    return (
                      <button
                        key={file.id}
                        onClick={() => setActiveFile(file.id)}
                        className={`w-full flex items-center gap-3 px-4 py-3 text-left transition-colors ${
                          activeFile === file.id
                            ? "bg-primary/10 text-primary border-r-2 border-primary"
                            : "text-muted-foreground hover:text-foreground hover:bg-muted/50"
                        }`}
                      >
                        <Icon className="h-4 w-4 flex-shrink-0" />
                        <div className="flex-1 min-w-0">
                          <div className="flex items-center gap-2">
                            <span className="font-mono text-sm font-medium">
                              {file.name}
                            </span>
                            <Badge variant="secondary" className="text-xs">
                              {file.badge}
                            </Badge>
                          </div>
                          <p className="text-xs text-muted-foreground mt-1 truncate">
                            {file.description}
                          </p>
                        </div>
                      </button>
                    );
                  })}
                </div>
              </CardContent>
            </Card>
          </div>

          {/* Code Display */}
          <div className="lg:col-span-8">
            {activeCodeFile && (
              <Card>
                <CardHeader>
                  <div className="flex items-center justify-between">
                    <div className="flex items-center gap-3">
                      <activeCodeFile.icon className="h-5 w-5 text-primary" />
                      <div>
                        <CardTitle className="flex items-center gap-2">
                          {activeCodeFile.name}
                          <Badge variant="outline">
                            {activeCodeFile.badge}
                          </Badge>
                        </CardTitle>
                        <p className="text-sm text-muted-foreground mt-1">
                          {activeCodeFile.description}
                        </p>
                      </div>
                    </div>
                    <Button
                      variant="outline"
                      size="sm"
                      onClick={() =>
                        copyToClipboard(
                          activeCodeFile.content,
                          activeCodeFile.id,
                        )
                      }
                    >
                      {copiedFile === activeCodeFile.id ? (
                        <Check className="h-4 w-4 text-green-600" />
                      ) : (
                        <Copy className="h-4 w-4" />
                      )}
                    </Button>
                  </div>
                </CardHeader>
                <CardContent className="p-0">
                  <div className="relative">
                    <SyntaxHighlighter
                      language={activeCodeFile.language}
                      style={syntaxTheme}
                      customStyle={{
                        margin: 0,
                        borderRadius: 0,
                        background: "transparent",
                        fontSize: "14px",
                        lineHeight: "1.5",
                        padding: "24px",
                      }}
                      className="border-t"
                      showLineNumbers={activeCodeFile.language !== "text"}
                      wrapLongLines
                    >
                      {activeCodeFile.content}
                    </SyntaxHighlighter>
                  </div>
                </CardContent>
              </Card>
            )}
          </div>
        </div>

        {/* Features */}
        <div className="mt-16">
          <div className="grid md:grid-cols-3 gap-8 max-w-4xl mx-auto">
            {features.map((feature, index) => (
              <div key={index} className="text-center">
                <div className="inline-flex items-center justify-center w-12 h-12 bg-primary/10 text-primary rounded-lg mb-4">
                  <feature.icon className="h-6 w-6" />
                </div>
                <h3 className="text-lg font-semibold mb-2">{feature.title}</h3>
                <p className="text-muted-foreground">{feature.description}</p>
              </div>
            ))}
          </div>
        </div>
      </div>
    </section>
  );
}
