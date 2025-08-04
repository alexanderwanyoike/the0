import { Metadata } from 'next';
import { NavigationMenu } from '@/components/landing-page/navigation-menu';

export const metadata: Metadata = {
  title: 'Install THE0 CLI | THE0 Platform',
  description:
    'Download and install the THE0 CLI for managing trading bots, deploying algorithms, and interacting with the platform.',
  openGraph: {
    title: 'Install THE0 CLI',
    description:
      'Get started with THE0 by installing our command-line interface.',
    type: 'website',
  },
  twitter: {
    card: 'summary_large_image',
    title: 'Install THE0 CLI',
    description:
      'Download and install the THE0 CLI for managing trading bots from the command line.',
  },
};

export default function InstallCliLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <div className="min-h-screen bg-background">
      <NavigationMenu showSearch={false} />
      {children}
    </div>
  );
}
