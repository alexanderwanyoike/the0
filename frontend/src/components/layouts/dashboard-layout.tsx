"use client";

import { ReactNode, useState } from "react";
import { usePathname } from "next/navigation";
import Link from "next/link";
import { useAuth } from "@/contexts/auth-context";
import { Sidebar } from "@/components/main-navigation";
import { ModeToggle } from "@/components/mode-toggle";
import { config } from "@/lib/config";

interface DashboardLayoutProps {
  children: ReactNode;
}

export default function DashboardLayout({ children }: DashboardLayoutProps) {
  const { user, logout } = useAuth();
  const [isSidebarCollapsed, setIsSidebarCollapsed] = useState(true);
  const pathname = usePathname();

  return (
    <div className="min-h-screen bg-background">
      {/* Sidebar */}
      <Sidebar
        navigation={[
          {
            name: "Dashboard",
            href: "/dashboard",
            icon: (props) => (
              <svg
                fill="none"
                viewBox="0 0 24 24"
                stroke="currentColor"
                {...props}
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6"
                />
              </svg>
            ),
          },
          {
            name: "AI Agent",
            href: "/ai-agent",
            icon: (props) => (
              <svg
                fill="none"
                viewBox="0 0 24 24"
                stroke="currentColor"
                {...props}
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M9.663 17h4.673M12 3v1m6.364 1.636l-.707.707M21 12h-1M4 12H3m3.343-5.657l-.707-.707m2.828 9.9a5 5 0 117.072 0l-.548.547A3.374 3.374 0 0014 18.469V19a2 2 0 11-4 0v-.531c0-.895-.356-1.754-.988-2.386l-.548-.547z"
                />
              </svg>
            ),
          },
          {
            name: "Custom Bots",
            href: "/custom-bots",
            icon: (props) => (
              <svg
                fill="none"
                viewBox="0 0 24 24"
                stroke="currentColor"
                {...props}
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M12 6V4m0 2a2 2 0 100 4m0-4a2 2 0 110 4m-6 8a2 2 0 100-4m0 4a2 2 0 100 4m0-4v2m0-6V4m6 6v10m6-2a2 2 0 100-4m0 4a2 2 0 100 4m0-4v2m0-6V4"
                />
              </svg>
            ),
          },
          {
            name: "Backtests",
            href: "/backtests",
            icon: (props) => (
              <svg
                fill="none"
                viewBox="0 0 24 24"
                stroke="currentColor"
                {...props}
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"
                />
              </svg>
            ),
          },
        ]}
        user={user}
        onLogout={logout}
        currentPath={pathname}
        isCollapsed={isSidebarCollapsed}
        onCollapsedChange={setIsSidebarCollapsed}
      />

      {/* Main content area */}
      <div
        className={`min-h-screen transition-all duration-300 ease-in-out ${
          isSidebarCollapsed ? "pl-[60px]" : "pl-64"
        }`}
      >
        {/* Top header */}
        <header
          className="fixed top-0 right-0 h-12 flex items-center justify-between px-6 border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60 z-10"
          style={{
            left: isSidebarCollapsed ? "60px" : "256px",
            transition: "left 300ms ease-in-out",
          }}
        >
          <div className="flex items-center gap-4">
            {/* Search functionality removed */}
          </div>
          <div className="flex items-center gap-4">
            <nav className="hidden md:flex items-center space-x-4">
              <Link
                href={config.docsUrl}
                className="text-sm font-medium text-muted-foreground hover:text-foreground transition-colors"
                target="_blank"
                rel="noopener noreferrer"
              >
                Docs
              </Link>
            </nav>
            <ModeToggle />
          </div>
        </header>

        {/* Main content */}
        <main className="pt-12 min-h-[calc(100vh-3rem)]">{children}</main>
      </div>
    </div>
  );
}
