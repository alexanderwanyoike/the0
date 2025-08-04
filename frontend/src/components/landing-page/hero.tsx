'use client';
import Link from 'next/link';
import { ArrowRight, TrendingUp, Search } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';

export function HeroSection() {
  const quickCategories = [
    'arbitrage',
    'swing-trading',
    'scalping',
    'momentum',
  ];

  return (
    <section className="pt-24 pb-16 md:pt-32 md:pb-24">
      <div className="container">
        <div className="text-center">
          <h1 className="text-4xl font-bold tracking-tight sm:text-5xl md:text-6xl lg:text-7xl">
            The0 <br />
            Automated trading bots <br />
            <span className="text-primary">made simple</span>
          </h1>
          <div className="inline-flex items-center gap-2 px-3 py-1 mb-6 text-sm bg-primary/10 border border-primary/20 rounded-full">
            <span className="w-2 h-2 bg-primary rounded-full animate-pulse"></span>
            Now in Beta
          </div>
          <p className="mt-6 text-xl text-muted-foreground max-w-3xl mx-auto">
            Set up trading strategies in minutes, not months. No coding
            required—just configure, deploy, and watch your investments grow on
            autopilot.
          </p>

          {/* Marketplace Discovery Section */}
          <div className="mt-12 max-w-4xl mx-auto">
            <h2 className="text-2xl font-semibold mb-4">
              Discover Trading Bots
            </h2>
            <p className="text-muted-foreground mb-8">
              Explore thousands of pre-built trading strategies from our
              community marketplace
            </p>

            {/* Quick Category Filters */}
            <div className="flex flex-wrap justify-center gap-2 mb-8">
              <span className="text-sm text-muted-foreground mr-2">
                Popular categories:
              </span>
              {quickCategories.map((category) => (
                <Link
                  key={category}
                  href={`/marketplace/search?category=${category}`}
                >
                  <Badge
                    variant="outline"
                    className="cursor-pointer hover:bg-primary hover:text-primary-foreground transition-colors"
                  >
                    {category}
                  </Badge>
                </Link>
              ))}
            </div>

            {/* Main CTA Buttons */}
            <div className="flex flex-wrap justify-center gap-4">
              <Link href="/marketplace/search">
                <Button size="lg" variant="outline" className="gap-2">
                  <Search className="h-4 w-4" />
                  Browse Marketplace
                </Button>
              </Link>
              <Link href="/register">
                <Button size="lg" className="gap-2">
                  Join Beta
                  <ArrowRight className="h-4 w-4" />
                </Button>
              </Link>
            </div>
          </div>

          {/* Value Proposition */}
          <div className="mt-16 grid grid-cols-1 md:grid-cols-3 gap-8 max-w-4xl mx-auto">
            <div className="text-center">
              <div className="h-12 w-12 mx-auto mb-4 bg-primary/10 rounded-lg flex items-center justify-center">
                <TrendingUp className="h-6 w-6 text-primary" />
              </div>
              <h3 className="font-semibold mb-2">Proven Strategies</h3>
              <p className="text-sm text-muted-foreground">
                Access battle-tested trading algorithms from experienced
                developers
              </p>
            </div>
            <div className="text-center">
              <div className="h-12 w-12 mx-auto mb-4 bg-primary/10 rounded-lg flex items-center justify-center">
                <Search className="h-6 w-6 text-primary" />
              </div>
              <h3 className="font-semibold mb-2">Easy Discovery</h3>
              <p className="text-sm text-muted-foreground">
                Find the perfect bot for your strategy with powerful search and
                filters
              </p>
            </div>
            <div className="text-center">
              <div className="h-12 w-12 mx-auto mb-4 bg-primary/10 rounded-lg flex items-center justify-center">
                <ArrowRight className="h-6 w-6 text-primary" />
              </div>
              <h3 className="font-semibold mb-2">One-Click Deploy</h3>
              <p className="text-sm text-muted-foreground">
                Install and deploy bots instantly with our streamlined workflow
              </p>
            </div>
          </div>
        </div>
      </div>
    </section>
  );
}
