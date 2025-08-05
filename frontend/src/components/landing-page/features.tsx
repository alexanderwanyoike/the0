'use client';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Code, TestTube, Zap, Container, BarChart3, Globe } from 'lucide-react';

const features = [
  {
    title: 'Custom Bot Development',
    description:
      'Build trading bots in Python or JavaScript (and more in the future) with any libraries you prefer. Full framework agnostic approach gives you complete flexibility.',
    icon: Code,
  },
  {
    title: 'Custom Backtesting',
    description:
      'Implement your own backtesting logic using any libraries you prefer. Test strategies with historical data before going live.',
    icon: TestTube,
  },
  {
    title: 'Real-time Execution',
    description:
      'Deploy scheduled or continuous trading bots with live market data. Support for both cron-based and event-driven execution models.',
    icon: Zap,
  },
  {
    title: 'Docker Ready',
    description:
      'Easy deployment with Docker Compose. Get the entire platform running locally in under 5 minutes with a single command.',
    icon: Container,
  },
  {
    title: 'Analytics Dashboard',
    description:
      'Monitor performance and track metrics in real-time. Comprehensive insights into your bot execution and trading results.',
    icon: BarChart3,
  },
  {
    title: 'Exchange Agnostic',
    description:
      'Design your bots to work with any trading platform. Connect to multiple exchanges and brokers through standardized APIs.',
    icon: Globe,
  },
];

export function FeaturesSection() {
  return (
    <section id="features" className="py-16 md:py-24">
      <div className="container">
        <div className="text-center mb-16">
          <h2 className="text-3xl font-bold">Why Choose the0</h2>
          <p className="mt-4 text-xl text-muted-foreground max-w-3xl mx-auto">
            Open source algorithmic trading platform built for developers and traders
          </p>
        </div>

        <div className="grid md:grid-cols-2 lg:grid-cols-3 gap-6 max-w-6xl mx-auto">
          {features.map((feature, index) => (
            <Card key={index} className="border">
              <CardHeader>
                <feature.icon className="h-8 w-8 mb-4 text-muted-foreground" />
                <CardTitle>{feature.title}</CardTitle>
              </CardHeader>
              <CardContent>
                <p className="text-muted-foreground">{feature.description}</p>
              </CardContent>
            </Card>
          ))}
        </div>
      </div>
    </section>
  );
}
