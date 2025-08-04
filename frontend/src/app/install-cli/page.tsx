import { Metadata } from 'next';
import { headers } from 'next/headers';
import { suggestPlatformFromHeaders } from '@/lib/install/platform-detection';
import { InstallationInterface } from '@/components/install/installation-interface';
import { VerificationSteps } from '@/components/install/verification-steps';
import { Terminal, Zap, Shield, HelpCircle } from 'lucide-react';
import Link from 'next/link';

export const metadata: Metadata = {
  title: 'Install THE0 CLI | THE0 Platform',
  description:
    'Download and install the THE0 CLI for managing trading bots, deploying algorithms, and interacting with the platform from the command line.',
  keywords: [
    'cli',
    'installation',
    'trading bots',
    'the0',
    'command line',
    'terminal',
    'developer tools',
  ],
  openGraph: {
    title: 'Install THE0 CLI',
    description:
      'Get started with THE0 by installing our powerful command-line interface for managing trading bots and algorithms.',
    type: 'website',
    images: [
      {
        url: '/og-install-cli.png',
        width: 1200,
        height: 630,
        alt: 'THE0 CLI Installation',
      },
    ],
  },
  twitter: {
    card: 'summary_large_image',
    title: 'Install THE0 CLI',
    description:
      'Download and install the THE0 CLI for managing trading bots from the command line.',
    images: ['/og-install-cli.png'],
  },
  robots: {
    index: true,
    follow: true,
  },
  alternates: {
    canonical: '/install-cli',
  },
};

// Server component for SEO and initial data
export default async function InstallCliPage() {
  // Server-side platform suggestion from request headers
  const headersList = await headers();
  const userAgent = headersList.get('user-agent');
  const suggestedPlatform = suggestPlatformFromHeaders(userAgent || undefined);

  return (
    <>
      {/* Hero Section */}
      <div className="border-b bg-gradient-to-b from-background to-muted/20">
        <div className="container mx-auto py-16 px-4 max-w-4xl">
          <div className="text-center space-y-6">
            <div className="flex justify-center">
              <div className="p-3 bg-primary/10 rounded-full">
                <Terminal className="h-12 w-12 text-primary" />
              </div>
            </div>

            <div className="space-y-4">
              <h1 className="text-4xl md:text-5xl font-bold tracking-tight">
                Install THE0 CLI
              </h1>
              <p className="text-xl text-muted-foreground max-w-2xl mx-auto leading-relaxed">
                The command-line interface for managing your trading bots,
                deploying custom algorithms, and interacting with the THE0
                platform ecosystem.
              </p>
            </div>

            <div className="flex justify-center space-x-6 text-sm text-muted-foreground">
              <div className="flex items-center space-x-2">
                <Zap className="h-4 w-4" />
                <span>Quick Install</span>
              </div>
              <div className="flex items-center space-x-2">
                <Shield className="h-4 w-4" />
                <span>Secure Download</span>
              </div>
              <div className="flex items-center space-x-2">
                <HelpCircle className="h-4 w-4" />
                <span>Full Support</span>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <div className="container mx-auto py-12 px-4 max-w-4xl space-y-12 overflow-hidden">
        {/* Platform Detection and Installation */}
        <section className="space-y-6">
          <div className="text-center space-y-2">
            <h2 className="text-2xl font-bold">Get Started</h2>
            <p className="text-muted-foreground">
              Choose your installation method below. We&apos;ll detect your
              platform automatically.
            </p>
          </div>
          <InstallationInterface suggestedPlatform={suggestedPlatform} />
        </section>

        {/* Verification Steps */}
        <section className="space-y-6">
          <div className="text-center space-y-2">
            <h2 className="text-2xl font-bold">Verify Installation</h2>
            <p className="text-muted-foreground">
              Follow these steps to confirm THE0 CLI was installed correctly.
            </p>
          </div>
          <VerificationSteps />
        </section>

        {/* Next Steps */}
        <section className="py-12 border-t">
          <div className="text-center space-y-6">
            <h2 className="text-2xl font-bold">What&apos;s Next?</h2>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mt-8">
              <div className="p-6 border rounded-lg space-y-3">
                <div className="h-12 w-12 bg-blue-100 rounded-lg flex items-center justify-center mx-auto">
                  <Terminal className="h-6 w-6 text-blue-600" />
                </div>
                <h3 className="font-semibold">Authenticate</h3>
                <p className="text-sm text-muted-foreground">
                  Connect your THE0 account to start managing your bots.
                </p>
                <code className="text-xs bg-muted px-2 py-1 rounded">
                  the0 auth login
                </code>
              </div>

              <div className="p-6 border rounded-lg space-y-3">
                <div className="h-12 w-12 bg-green-100 rounded-lg flex items-center justify-center mx-auto">
                  <Zap className="h-6 w-6 text-green-600" />
                </div>
                <h3 className="font-semibold">Explore Commands</h3>
                <p className="text-sm text-muted-foreground">
                  Discover all available CLI commands and features.
                </p>
                <code className="text-xs bg-muted px-2 py-1 rounded">
                  the0 --help
                </code>
              </div>

              <div className="p-6 border rounded-lg space-y-3">
                <div className="h-12 w-12 bg-purple-100 rounded-lg flex items-center justify-center mx-auto">
                  <HelpCircle className="h-6 w-6 text-purple-600" />
                </div>
                <h3 className="font-semibold">Read Docs</h3>
                <p className="text-sm text-muted-foreground">
                  Learn advanced features and best practices.
                </p>
                <Link
                  href="/docs/CLI/installation"
                  className="text-xs text-blue-600 hover:underline"
                >
                  View Documentation →
                </Link>
              </div>
            </div>
          </div>
        </section>
      </div>
    </>
  );
}
