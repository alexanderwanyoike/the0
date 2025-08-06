"use client";
import Link from "next/link";
import { ArrowRight, Code, TestTube, Zap, BarChart3 } from "lucide-react";
import { Button } from "@/components/ui/button";

export function HeroSection() {
  return (
    <section className="pt-24 pb-16 md:pt-32 md:pb-24">
      <div className="container">
        <div className="text-center">
          <h1 className="text-4xl font-bold tracking-tight sm:text-5xl md:text-6xl lg:text-7xl">
            the0 <br />
            Open source algorithmic <br />
            <span className="text-primary">trading platform</span>
          </h1>
          <p className="mt-6 text-xl text-muted-foreground max-w-3xl mx-auto">
            Create, deploy, and manage trading bots with ease. Whether you're
            building a simple DCA strategy or complex multi-asset arbitrage
            algorithms, the0 provides the infrastructure and tools you need.
          </p>

          {/* Main CTA Buttons */}
          <div className="flex flex-wrap justify-center gap-4 mt-12">
            <Link href="/login">
              <Button size="lg" variant="outline">
                Sign In
              </Button>
            </Link>
            <Link href="/register">
              <Button size="lg" className="gap-2">
                Get Started
                <ArrowRight className="h-4 w-4" />
              </Button>
            </Link>
          </div>

          {/* Key Features */}
          <div className="mt-16 grid grid-cols-1 md:grid-cols-4 gap-8 max-w-6xl mx-auto">
            <div className="text-center">
              <div className="h-12 w-12 mx-auto mb-4 bg-primary/10 rounded-lg flex items-center justify-center">
                <Code className="h-6 w-6 text-primary" />
              </div>
              <h3 className="font-semibold mb-2">Custom Bot Development</h3>
              <p className="text-sm text-muted-foreground">
                Build bots in Python or JavaScript (and more in the future) with
                any libraries you prefer
              </p>
            </div>
            <div className="text-center">
              <div className="h-12 w-12 mx-auto mb-4 bg-primary/10 rounded-lg flex items-center justify-center">
                <TestTube className="h-6 w-6 text-primary" />
              </div>
              <h3 className="font-semibold mb-2">Custom Backtesting</h3>
              <p className="text-sm text-muted-foreground">
                Test strategies with historical data before going live. Define
                your own custom backtesting implementation with whatever
                libraries you prefer.
              </p>
            </div>
            <div className="text-center">
              <div className="h-12 w-12 mx-auto mb-4 bg-primary/10 rounded-lg flex items-center justify-center">
                <Zap className="h-6 w-6 text-primary" />
              </div>
              <h3 className="font-semibold mb-2">Real-time Execution</h3>
              <p className="text-sm text-muted-foreground">
                Deploy scheduled or continuous trading bots with live data
              </p>
            </div>
            <div className="text-center">
              <div className="h-12 w-12 mx-auto mb-4 bg-primary/10 rounded-lg flex items-center justify-center">
                <BarChart3 className="h-6 w-6 text-primary" />
              </div>
              <h3 className="font-semibold mb-2">Analytics Dashboard</h3>
              <p className="text-sm text-muted-foreground">
                Monitor performance and track metrics in real-time
              </p>
            </div>
          </div>
        </div>
      </div>
    </section>
  );
}
