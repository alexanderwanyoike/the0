"use client";

import { ReactNode } from "react";
import {
  Group,
  Panel,
  Separator,
  useDefaultLayout,
} from "react-resizable-panels";
import { cn } from "@/lib/utils";

interface ResizableSidebarLayoutProps {
  sidebar: ReactNode;
  children: ReactNode;
  className?: string;
}

// The library's default storage is a bare `localStorage` reference, which
// throws during SSR of client components; substitute a no-op there so the
// server renders the default layout and the client restores the saved one.
const ssrSafeStorage =
  typeof window === "undefined"
    ? { getItem: () => null, setItem: () => {} }
    : window.localStorage;

/**
 * Desktop dashboard shell: a resizable bot-list sidebar next to the main
 * content. The sidebar width is draggable (and keyboard-resizable via the
 * separator) and persists across reloads.
 */
export function ResizableSidebarLayout({
  sidebar,
  children,
  className,
}: ResizableSidebarLayoutProps) {
  const { defaultLayout, onLayoutChanged } = useDefaultLayout({
    id: "dashboard-bot-list",
    storage: ssrSafeStorage,
  });

  return (
    <Group
      orientation="horizontal"
      defaultLayout={defaultLayout}
      onLayoutChanged={onLayoutChanged}
      className={cn("h-full", className)}
    >
      <Panel
        id="bot-list"
        defaultSize={280}
        minSize={220}
        maxSize={480}
        className="border-r"
      >
        {sidebar}
      </Panel>
      {/* Invisible until hovered/focused; the sidebar's border-r draws the
          resting divider line. -mx widens the grab area past the 1px line. */}
      <Separator className="relative z-10 w-1 -mx-0.5 bg-transparent transition-colors hover:bg-primary/40 focus-visible:bg-primary/40 focus-visible:outline-none" />
      <Panel id="dashboard-main">{children}</Panel>
    </Group>
  );
}
