"use client";

import { ReactNode, useEffect } from "react";
import { useRouter } from "next/navigation";
import { usePathname } from "next/navigation";
import { useAuth } from "@/contexts/auth-context";
import DashboardLayout from "@/components/layouts/dashboard-layout";
import {
  DashboardBotsProvider,
  useDashboardBots,
} from "@/contexts/dashboard-bots-context";
import { BotListPanel } from "@/components/dashboard/bot-list-panel";
import { useMediaQuery } from "@/hooks/use-media-query";

function DashboardInner({ children }: { children: ReactNode }) {
  const { bots } = useDashboardBots();
  const router = useRouter();
  const pathname = usePathname();
  const isDesktop = useMediaQuery("(min-width: 1280px)");

  // Extract active bot ID from URL
  const pathParts = pathname.split("/");
  const activeBotId = pathParts.length >= 3 ? pathParts[2] : null;

  const handleSelectBot = (botId: string) => {
    router.push(`/dashboard/${botId}`);
  };

  // Wait for media query to resolve
  if (isDesktop === null) {
    return (
      <div className="h-[calc(100vh-3rem)]">
        <main className="h-full overflow-auto">{children}</main>
      </div>
    );
  }

  // Desktop: side panel + content
  if (isDesktop) {
    return (
      <div className="flex h-[calc(100vh-3rem)]">
        <aside className="w-[220px] border-r flex-shrink-0">
          <BotListPanel
            bots={bots}
            activeBotId={activeBotId}
            onSelectBot={handleSelectBot}
            className="h-full"
          />
        </aside>
        <main className="flex-1 overflow-auto">{children}</main>
      </div>
    );
  }

  // Non-desktop: children only (list page or detail page handles its own layout)
  return (
    <div className="h-[calc(100vh-3rem)]">
      <main className="h-full overflow-auto">{children}</main>
    </div>
  );
}

function AuthGate({ children }: { children: ReactNode }) {
  const { user, loading } = useAuth();
  const router = useRouter();

  useEffect(() => {
    if (!loading && !user) {
      router.replace("/login");
    }
  }, [user, loading, router]);

  if (loading || !user) return null;

  return <>{children}</>;
}

export default function Layout({ children }: { children: ReactNode }) {
  return (
    <DashboardLayout>
      <AuthGate>
        <DashboardBotsProvider>
          <DashboardInner>{children}</DashboardInner>
        </DashboardBotsProvider>
      </AuthGate>
    </DashboardLayout>
  );
}
