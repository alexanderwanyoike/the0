'use client';

import { useState, useEffect } from 'react';
import { DocsSidebar } from '@/components/docs/docs-sidebar';
import { DocsSearch } from '@/components/docs/docs-search';
import { DocsBreadcrumbs } from '@/components/docs/docs-breadcrumbs';
import { DocItem } from '@/lib/docs/file-system';
import { Button } from '@/components/ui/button';
import { ModeToggle } from '@/components/mode-toggle';
import { Menu, PanelLeftOpen } from 'lucide-react';

interface DocsLayoutProps {
  children: React.ReactNode;
  currentPath?: string[];
}

export function DocsLayout({ children, currentPath = [] }: DocsLayoutProps) {
  const [sidebarOpen, setSidebarOpen] = useState(true);
  const [navigationData, setNavigationData] = useState<DocItem[]>([]);
  const [isMobile, setIsMobile] = useState(false);

  useEffect(() => {
    const checkMobile = () => {
      setIsMobile(window.innerWidth < 768);
      if (window.innerWidth < 768) {
        setSidebarOpen(false);
      }
    };

    checkMobile();
    window.addEventListener('resize', checkMobile);
    return () => window.removeEventListener('resize', checkMobile);
  }, []);

  useEffect(() => {
    fetch('/api/docs/navigation')
      .then((res) => res.json())
      .then((data) => {
        if (data.success) {
          setNavigationData(data.data);
        }
      })
      .catch(console.error);
  }, []);

  const toggleSidebar = () => {
    setSidebarOpen(!sidebarOpen);
  };

  return (
    <div className="min-h-screen bg-background">
      {/* Mobile overlay */}
      {isMobile && sidebarOpen && (
        <div
          className="fixed inset-0 bg-black/50 z-40 md:hidden"
          onClick={() => setSidebarOpen(false)}
        />
      )}

      {/* Sidebar */}
      <DocsSidebar
        navigation={navigationData}
        isOpen={sidebarOpen}
        onToggle={toggleSidebar}
        currentPath={currentPath}
        isMobile={isMobile}
      />

      {/* Toggle button for desktop when sidebar is closed */}
      {!sidebarOpen && !isMobile && (
        <Button
          variant="ghost"
          size="icon"
          onClick={toggleSidebar}
          className="fixed left-4 top-20 z-40 h-8 w-8 rounded-full border bg-background hover:bg-accent hover:text-accent-foreground"
        >
          <PanelLeftOpen className="h-4 w-4" />
        </Button>
      )}

      {/* Main content */}
      <div
        className={`transition-all duration-300 ${sidebarOpen && !isMobile ? 'pl-72' : 'pl-0'}`}
      >
        {/* Header */}
        <header className="sticky top-0 z-30 bg-background/95 backdrop-blur border-b px-4 py-4">
          <div className="flex items-center justify-between">
            <div className="flex items-center gap-4">
              <Button
                variant="ghost"
                size="icon"
                onClick={toggleSidebar}
                className="md:hidden"
              >
                <Menu className="h-5 w-5" />
              </Button>
              <h1 className="text-xl font-semibold hidden md:block">Docs</h1>
            </div>
            <div className="flex items-center gap-2">
              <DocsSearch />
              <ModeToggle />
            </div>
          </div>
          {currentPath.length > 0 && (
            <div className="mt-4">
              <DocsBreadcrumbs path={currentPath} />
            </div>
          )}
        </header>

        {/* Content */}
        <main className="container mx-auto px-4 py-8 max-w-4xl">
          {children}
        </main>
      </div>
    </div>
  );
}
