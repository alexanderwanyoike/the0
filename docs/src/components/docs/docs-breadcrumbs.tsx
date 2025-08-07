"use client";

import React from "react";
import Link from "next/link";
import { ChevronRight, Home } from "lucide-react";
import { cn } from "@/lib/utils";

interface DocsBreadcrumbsProps {
  path: string[];
  className?: string;
}

export const DocsBreadcrumbs: React.FC<DocsBreadcrumbsProps> = ({
  path,
  className,
}) => {
  const formatSegment = (segment: string) => {
    return segment
      .replace(/[-_]/g, " ")
      .replace(/\b\w/g, (char) => char.toUpperCase());
  };

  const buildPath = (index: number) => {
    return "/docs/" + path.slice(0, index + 1).join("/");
  };

  return (
    <nav className={cn("flex items-center space-x-1 text-sm", className)}>
      <Link
        href="/docs"
        className="flex items-center text-muted-foreground hover:text-foreground transition-colors"
      >
        <Home className="h-4 w-4" />
        <span className="sr-only">Documentation Home</span>
      </Link>

      {path.map((segment, index) => (
        <React.Fragment key={index}>
          <ChevronRight className="h-4 w-4 text-muted-foreground" />
          {index === path.length - 1 ? (
            <span className="font-medium text-foreground">
              {formatSegment(segment)}
            </span>
          ) : (
            <Link
              href={buildPath(index)}
              className="text-muted-foreground hover:text-foreground transition-colors"
            >
              {formatSegment(segment)}
            </Link>
          )}
        </React.Fragment>
      ))}
    </nav>
  );
};
