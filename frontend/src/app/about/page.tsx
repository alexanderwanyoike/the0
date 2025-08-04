import React from 'react';
import { Mail, MapPin, Phone, Github, Twitter, Linkedin } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { NavigationMenu } from '@/components/landing-page/navigation-menu';

export default function AboutPage() {
  return (
    <div className="min-h-screen bg-background">
      <NavigationMenu />

      {/* Hero Section */}
      <div className="py-16 md:py-24">
        <div className="container max-w-4xl">
          <div className="text-center">
            <div className="mb-6 flex justify-center">
              <div className="bg-primary w-12 h-12 rounded flex items-center justify-center">
                <span className="text-primary-foreground font-mono font-bold text-2xl">
                  0
                </span>
              </div>
            </div>
            <h1 className="text-4xl font-bold mb-4">About the0</h1>
            <p className="text-xl text-muted-foreground max-w-2xl mx-auto">
              We make professional trading automation accessible to everyone.
              Our platform handles the complexity while keeping you in complete
              control. Choose from proven strategies, configure through our
              simple interface, and deploy automated trading bots—no coding
              required.
            </p>
          </div>
        </div>
      </div>

      {/* Mission Section */}
      <div className="py-16 bg-muted/50">
        <div className="container max-w-4xl">
          <div className="text-center mb-12">
            <h2 className="text-2xl font-bold mb-4">Our Mission</h2>
            <p className="text-lg text-muted-foreground">
              We&#39;re democratizing professional trading automation. Our goal
              is to give every trader—from beginners to experts—access to
              sophisticated strategies that were once exclusive to institutions.
            </p>
          </div>

          <div className="grid md:grid-cols-3 gap-8 text-center">
            <div>
              <div className="bg-primary/10 rounded-lg p-6 mb-4">
                <h3 className="font-bold mb-2">Simple & Intuitive</h3>
                <p className="text-muted-foreground">
                  Set up powerful trading bots through our user-friendly
                  interface. Choose strategies, configure parameters, and
                  deploy—all without writing code.
                </p>
              </div>
            </div>
            <div>
              <div className="bg-primary/10 rounded-lg p-6 mb-4">
                <h3 className="font-bold mb-2">Secure & Trustworthy</h3>
                <p className="text-muted-foreground">
                  Your funds stay in your brokerage account. We execute trades
                  securely on your behalf through encrypted connections, never
                  holding your money.
                </p>
              </div>
            </div>
            <div>
              <div className="bg-primary/10 rounded-lg p-6 mb-4">
                <h3 className="font-bold mb-2">Proven Strategies</h3>
                <p className="text-muted-foreground">
                  Access a library of tested trading strategies—from
                  conservative dollar-cost averaging to dynamic momentum
                  trading—all ready to deploy.
                </p>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* How We Work Section */}
      <div className="py-16">
        <div className="container max-w-4xl">
          <div className="text-center mb-12">
            <h2 className="text-2xl font-bold mb-4">How We Work</h2>
            <p className="text-muted-foreground">
              Our platform bridges the gap between complex algorithmic trading
              and everyday investors.
            </p>
          </div>

          <div className="grid md:grid-cols-2 gap-8">
            <div className="space-y-6">
              <div>
                <h3 className="font-bold mb-2 text-primary">
                  Choose Your Strategy
                </h3>
                <p className="text-muted-foreground">
                  Browse our curated library of proven trading strategies. Each
                  strategy is thoroughly tested and comes with clear
                  explanations of how it works and what risks are involved.
                </p>
              </div>

              <div>
                <h3 className="font-bold mb-2 text-primary">
                  Simple Configuration
                </h3>
                <p className="text-muted-foreground">
                  Configure your bot through our intuitive interface. Set your
                  budget, choose your assets, pick your schedule, and customize
                  risk parameters— all with simple sliders and dropdowns.
                </p>
              </div>
            </div>

            <div className="space-y-6">
              <div>
                <h3 className="font-bold mb-2 text-primary">
                  One-Click Deployment
                </h3>
                <p className="text-muted-foreground">
                  Connect your brokerage account and launch your bot instantly.
                  Your automated strategy starts working immediately, executing
                  trades based on your configured rules.
                </p>
              </div>

              <div>
                <h3 className="font-bold mb-2 text-primary">Full Control</h3>
                <p className="text-muted-foreground">
                  Monitor performance through your dashboard. View profits,
                  adjust settings, pause strategies, or modify parameters
                  anytime. You&#39;re always in complete control of your
                  automated investments.
                </p>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Contact Section */}
      <div className="py-16 bg-muted/50">
        <div className="container max-w-4xl">
          <div className="text-center mb-12">
            <h2 className="text-2xl font-bold mb-4">Contact Us</h2>
            <p className="text-muted-foreground">
              Have questions? Our team is here to help.
            </p>
          </div>

          <div className="grid md:grid-cols-2 gap-8">
            <div className="space-y-6">
              <div className="flex items-center gap-3">
                <Mail className="h-5 w-5 text-muted-foreground" />
                <div>
                  <p className="font-medium">Email</p>
                  <a
                    href="mailto:contact@alphaneuron.dev"
                    className="text-primary hover:text-primary/80"
                  >
                    contact@alphanueron.net
                  </a>
                </div>
              </div>

              <div className="flex items-center gap-3">
                <Phone className="h-5 w-5 text-muted-foreground" />
                <div>
                  <p className="font-medium">Phone</p>
                  <a
                    href="tel:+447376195527"
                    className="text-primary hover:text-primary/80"
                  >
                    (+44) 737619 5527
                  </a>
                </div>
              </div>

              <div className="flex items-center gap-3">
                <MapPin className="h-5 w-5 text-muted-foreground" />
                <div>
                  <p className="font-medium">Address</p>
                  <p className="text-muted-foreground">
                    406 4 Box Works Worsley Street
                    <br />
                    Manchester, M15 4NU
                    <br />
                  </p>
                </div>
              </div>
            </div>

            <div className="space-y-6">
              <h3 className="font-medium mb-4">Connect With Us</h3>
              <div className="flex gap-4">
                <Button variant="outline" size="icon" asChild>
                  <a
                    href="https://x.com/the0dev"
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    <Twitter className="h-4 w-4" />
                  </a>
                </Button>
                <Button variant="outline" size="icon" asChild>
                  <a
                    href="https://www.linkedin.com/company/alpha-neuron"
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    <Linkedin className="h-4 w-4" />
                  </a>
                </Button>
              </div>

              <div className="border rounded-lg p-6 bg-background">
                <h4 className="font-medium mb-2">Support Hours</h4>
                <p className="text-muted-foreground mb-4">
                  Monday - Friday
                  <br />
                  9:00 AM - 6:00 PM (GMT)
                </p>
                <Button asChild>
                  <a href="mailto:support@alphaneuron.net">Contact Support</a>
                </Button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
