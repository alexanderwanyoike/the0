'use client';
import { useSelectedLayoutSegment } from 'next/navigation';
import Link from 'next/link';
import DashboardLayout from '@/components/layouts/dashboard-layout';
import { cn } from '@/lib/utils';
import React from 'react';

export default function SettingsLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  const segment = useSelectedLayoutSegment();


  return (
    <DashboardLayout>
      <div className="container max-w-4xl py-4 px-4 sm:py-8 sm:px-6">
        {/* Page Header */}
        <div className="mb-6 sm:mb-8">
          <h1 className="text-xl sm:text-2xl font-medium">Settings</h1>
          <p className="text-sm text-muted-foreground mt-1">
            Manage your account settings and preferences
          </p>
        </div>

        {/* Navigation */}
        <div className="sticky top-0 z-10 bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60 pb-3 sm:pb-4 border-b mb-6 sm:mb-8 -mx-4 px-4 sm:mx-0 sm:px-0">
          <nav className="flex space-x-2 sm:space-x-4 overflow-x-auto">
            <Link
              href="/settings/profile"
              className={cn(
                'px-3 sm:px-4 py-2 text-sm whitespace-nowrap transition-colors hover:text-primary',
                segment === 'profile'
                  ? 'border-b-2 border-primary font-medium text-primary'
                  : 'text-muted-foreground',
              )}
            >
              Profile
            </Link>
            <Link
              href="/settings/api-settings"
              className={cn(
                'px-3 sm:px-4 py-2 text-sm whitespace-nowrap transition-colors hover:text-primary',
                segment === 'api-settings'
                  ? 'border-b-2 border-primary font-medium text-primary'
                  : 'text-muted-foreground',
              )}
            >
              API
            </Link>
          </nav>
        </div>

        <div className="space-y-6 sm:space-y-8">{children}</div>
      </div>
    </DashboardLayout>
  );
}
