import { RefreshCw } from 'lucide-react';

export default function MaintenancePage() {
  return (
    <div className="min-h-screen bg-background flex items-center justify-center px-4">
      <div className="max-w-2xl mx-auto text-center">
        {/* Logo/Brand */}
        <div className="mb-8">
          <h1 className="text-4xl font-bold tracking-tight mb-2">the0</h1>
          <div className="inline-flex items-center gap-2 px-3 py-1 text-sm bg-orange-100 dark:bg-orange-900/20 border border-orange-200 dark:border-orange-800 rounded-full">
            <RefreshCw className="w-3 h-3 animate-spin text-orange-600" />
            <span className="text-orange-800 dark:text-orange-200">
              Major Pivot in Progress
            </span>
          </div>
        </div>

        {/* Main Message */}
        <div className="mb-12">
          <h2 className="text-3xl font-semibold mb-4">
            We&apos;re Building Something Amazing
          </h2>
          <p className="text-xl text-muted-foreground mb-6">
            the0 is undergoing a major pivot to better serve our trading
            community. We&apos;re reimagining how automated trading should work
            and will be back soon with an even more powerful platform.
          </p>
          <p className="text-muted-foreground">
            Thank you for your patience as we work to deliver the best possible
            experience.
          </p>
        </div>

        {/* Timeline */}
        <div className="mb-8">
          <h3 className="text-lg font-semibold mb-3">Expected Timeline</h3>
          <p className="text-muted-foreground">
            We&apos;re aiming to be back within the next few weeks. Follow our
            updates for the latest progress.
          </p>
        </div>

        {/* Contact */}
        <div className="text-sm text-muted-foreground">
          <p>
            Questions? Reach out to us at{' '}
            <a
              href="mailto:support@alphanueron.net"
              className="text-primary hover:underline"
            >
              support@alphanueron.net
            </a>
          </p>
        </div>

        {/* Social Links or Additional Info */}
        <div className="mt-8 pt-8 border-t border-border">
          <p className="text-xs text-muted-foreground">
            © 2025 AlphaNeuron. All rights reserved.
          </p>
        </div>
      </div>
    </div>
  );
}
