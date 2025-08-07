"use client";

import React, { useState, useEffect } from "react";
import Link from "next/link";
import { usePathname } from "next/navigation";
import {
  ChevronRight,
  ChevronDown,
  FileText,
  Folder,
  PanelLeftClose,
} from "lucide-react";
import { cn } from "@/lib/utils";
import { Button } from "@/components/ui/button";
import { ScrollArea } from "@/components/ui/scroll-area";
import { DocItem } from "@/lib/docs/file-system";
import { APP_NAME } from "@/lib/constants";

interface DocsSidebarProps {
  navigation: DocItem[];
  isOpen: boolean;
  onToggle: () => void;
  currentPath?: string[];
  isMobile?: boolean;
}

export const DocsSidebar: React.FC<DocsSidebarProps> = ({
  navigation,
  isOpen,
  onToggle,
  currentPath = [],
  isMobile = false,
}) => {
  const [expandedFolders, setExpandedFolders] = useState<Set<string>>(
    new Set(),
  );
  const pathname = usePathname();

  // Helper function to collect all folder paths
  const getAllFolderPaths = (items: DocItem[]): string[] => {
    const paths: string[] = [];
    const traverse = (items: DocItem[]) => {
      items.forEach((item) => {
        if (item.type === "folder") {
          paths.push(item.path);
          if (item.children) {
            traverse(item.children);
          }
        }
      });
    };
    traverse(items);
    return paths;
  };

  // Initialize all folders as expanded when navigation data is available
  useEffect(() => {
    if (navigation.length > 0) {
      setExpandedFolders(new Set(getAllFolderPaths(navigation)));
    }
  }, [navigation]);

  const toggleFolder = (path: string) => {
    const newExpanded = new Set(expandedFolders);
    if (newExpanded.has(path)) {
      newExpanded.delete(path);
    } else {
      newExpanded.add(path);
    }
    setExpandedFolders(newExpanded);
  };

  const isActive = (itemPath: string) => {
    return pathname === `/docs/${itemPath}`;
  };

  const renderNavItem = (item: DocItem, level: number = 0) => {
    const isExpanded = expandedFolders.has(item.path);
    const itemActive = isActive(item.path);
    const paddingLeft = level * 16 + 16;

    if (item.type === "folder") {
      return (
        <div key={item.path}>
          <button
            onClick={() => toggleFolder(item.path)}
            className={cn(
              "w-full flex items-center gap-2 px-4 py-2 text-sm hover:bg-muted/50 transition-colors",
              isExpanded && "bg-muted/30",
            )}
            style={{ paddingLeft }}
          >
            <Folder className="h-4 w-4 text-muted-foreground" />
            <span className="flex-1 text-left">{item.title}</span>
            {isExpanded ? (
              <ChevronDown className="h-4 w-4 text-muted-foreground" />
            ) : (
              <ChevronRight className="h-4 w-4 text-muted-foreground" />
            )}
          </button>
          {isExpanded && item.children && (
            <div>
              {item.children.map((child) => renderNavItem(child, level + 1))}
            </div>
          )}
        </div>
      );
    }

    return (
      <Link
        key={item.path}
        href={`/docs/${item.path}`}
        className={cn(
          "flex items-center gap-2 px-4 py-2 text-sm hover:bg-muted/50 transition-colors",
          itemActive && "bg-primary/10 text-primary border-r-2 border-primary",
        )}
        style={{ paddingLeft }}
      >
        <FileText className="h-4 w-4 text-muted-foreground" />
        <span className="flex-1">{item.title}</span>
      </Link>
    );
  };

  return (
    <>
      <div
        className={cn(
          "fixed inset-y-0 left-0 z-50 flex flex-col bg-background border-r border-border transition-transform duration-300",
          isOpen ? "translate-x-0" : "-translate-x-full",
          isMobile ? "w-80" : "w-72",
        )}
      >
        {/* Header */}
        <div className="flex items-center justify-between p-4 border-b border-border">
          <Link
            href="/"
            className="flex items-center space-x-2 hover:opacity-80 transition-opacity"
          >
            <div className="flex h-8 w-8 items-center justify-center rounded bg-primary">
              <span className="font-mono text-lg font-bold text-primary-foreground">
                0
              </span>
            </div>
            {!isMobile && (
              <div className="flex items-center gap-2">
                <span className="font-mono text-lg font-bold">{APP_NAME}</span>
                <span className="px-1 py-0.5 text-[8px] font-medium bg-orange-100 text-orange-800 dark:bg-orange-900/30 dark:text-orange-300 rounded-full border border-orange-200 dark:border-orange-800">
                  BETA
                </span>
              </div>
            )}
          </Link>
          <Button
            variant="ghost"
            size="icon"
            onClick={onToggle}
            className="h-8 w-8"
          >
            <PanelLeftClose className="h-4 w-4" />
          </Button>
        </div>

        {/* Navigation */}
        <ScrollArea className="flex-1">
          <div className="p-2">
            {navigation.length > 0 ? (
              navigation.map((item) => renderNavItem(item))
            ) : (
              <div className="p-4 text-center text-muted-foreground">
                <p className="text-sm">No documentation found</p>
                <p className="text-xs mt-1">
                  Add markdown files to <code>src/docs/</code>
                </p>
              </div>
            )}
          </div>
        </ScrollArea>
      </div>
    </>
  );
};
