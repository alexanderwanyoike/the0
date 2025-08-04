'use client';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Settings, Library, LayoutDashboard, Shield } from 'lucide-react';

const features = [
  {
    title: 'Simple Configuration',
    description:
      'Set up your trading bots through an intuitive interface. Choose your strategy, pick your asset, and customize parameters—all without writing a single line of code.',
    icon: Settings,
  },
  {
    title: 'Smart Dashboard',
    description:
      'Monitor all your bots from one place. Track performance, adjust settings, and get insights into your automated trading portfolio.',
    icon: LayoutDashboard,
  },
  {
    title: 'Strategy Library',
    description:
      'Choose from proven trading strategies like dollar-cost averaging, momentum trading, and mean reversion—all tested and ready to deploy.',
    icon: Library,
  },
  {
    title: 'Secure & Reliable',
    description:
      'Your funds stay in your brokerage account. We never hold your money, just execute trades securely on your behalf through encrypted connections.',
    icon: Shield,
  },
];

export function FeaturesSection() {
  return (
    <section id="features" className="py-16 md:py-24">
      <div className="container">
        <div className="text-center mb-16">
          <h2 className="text-3xl font-bold">Why Choose the0</h2>
          <p className="mt-4 text-xl text-muted-foreground max-w-3xl mx-auto">
            Professional-grade trading automation, designed for everyone
          </p>
        </div>

        <div className="grid md:grid-cols-2 gap-6 max-w-4xl mx-auto">
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
