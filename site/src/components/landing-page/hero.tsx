"use client";
import Link from "next/link";
import { Code, TestTube, Zap, BarChart3, Github } from "lucide-react";
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
            Create, deploy, and manage trading bots with ease. Whether
            you&apos;re building a simple DCA strategy or complex multi-asset
            arbitrage algorithms, the0 provides the infrastructure and tools you
            need.
          </p>

          {/* Main CTA Buttons */}
          <div className="flex flex-wrap justify-center gap-4 mt-12">
            <Link
              href="https://github.com/alexanderwanyoike/the0"
              target="_blank"
            >
              <Button size="lg" className="gap-2">
                <Github className="h-4 w-4" />
                View on GitHub
              </Button>
            </Link>
          </div>
        </div>
      </div>
    </section>
  );
}
