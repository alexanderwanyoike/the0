"use client";

import * as React from "react";
import Link from "next/link";
import { Button } from "@/components/ui/button";
import {
  NavigationMenu as UINavigationMenu,
  NavigationMenuItem,
  NavigationMenuLink,
  NavigationMenuList,
  navigationMenuTriggerStyle,
} from "@/components/ui/navigation-menu";
import {
  Sheet,
  SheetContent,
  SheetTrigger,
  SheetTitle,
} from "@/components/ui/sheet";
import { ModeToggle } from "@/components/mode-toggle";
import { useAuth } from "@/contexts/auth-context";
import { Skeleton } from "@/components/ui/skeleton";
import { Menu, User2 } from "lucide-react";
import { cn } from "@/lib/utils";
import { config } from "@/lib/config";

interface NavigationMenuProps {
  showSearch?: boolean;
}

export function NavigationMenu({ showSearch = true }: NavigationMenuProps) {
  const { user, loading } = useAuth();
  const [open, setOpen] = React.useState(false);

  const NavItems = () => (
    <>
      <Link
        href="/about"
        className="text-sm font-medium text-muted-foreground transition-colors hover:text-foreground"
        onClick={() => setOpen(false)}
      >
        About
      </Link>
      <Link
        href={config.docsUrl}
        className="text-sm font-medium text-muted-foreground transition-colors hover:text-foreground"
        onClick={() => setOpen(false)}
        target="_blank"
        rel="noopener noreferrer"
      >
        Docs
      </Link>
    </>
  );

  return (
    <header className="sticky top-0 z-50 w-full border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60">
      <div className="container flex h-16 max-w-screen-2xl items-center">
        {/* Logo */}
        <Link href="/" className="flex items-center space-x-2 mr-8">
          <div className="bg-primary h-8 w-8 rounded-md flex items-center justify-center">
            <span className="text-primary-foreground font-mono font-bold text-sm">
              0
            </span>
          </div>
          <div className="flex items-center space-x-2">
            <span className="text-xl font-bold tracking-tight">the0</span>
            <span className="px-1.5 py-0.5 text-[10px] font-medium bg-orange-100 text-orange-800 dark:bg-orange-900/30 dark:text-orange-300 rounded-md border border-orange-200 dark:border-orange-800">
              BETA
            </span>
          </div>
        </Link>

        {/* Desktop Navigation */}
        <nav className="hidden lg:flex items-center space-x-6 text-sm font-medium">
          <Link
            href="/about"
            className="transition-colors hover:text-foreground/80 text-foreground/60"
          >
            About
          </Link>
          <Link
            href={config.docsUrl}
            className="transition-colors hover:text-foreground/80 text-foreground/60"
            target="_blank"
            rel="noopener noreferrer"
          >
            Docs
          </Link>
        </nav>

        {/* Search functionality removed */}

        {/* Right side items */}
        <div className="flex items-center space-x-2 ml-auto">
          {/* Desktop auth buttons */}
          <div className="hidden sm:flex sm:items-center sm:space-x-2">
            <ModeToggle />
            {loading ? (
              <div className="flex items-center space-x-2">
                <Skeleton className="h-9 w-16" />
                <Skeleton className="h-9 w-20" />
              </div>
            ) : user ? (
              <div className="flex items-center space-x-2">
                <Button variant="ghost" size="sm" className="gap-2">
                  <User2 className="h-4 w-4" />
                  <span className="hidden lg:inline">
                    {user.username || "User"}
                  </span>
                </Button>
                <Button asChild size="sm">
                  <Link href="/dashboard">Dashboard</Link>
                </Button>
              </div>
            ) : (
              <div className="flex items-center space-x-2">
                <Button variant="ghost" asChild size="sm">
                  <Link href="/login">Log in</Link>
                </Button>
                <Button
                  asChild
                  size="sm"
                  className="bg-primary hover:bg-primary/90"
                >
                  <Link href="/register">Sign up</Link>
                </Button>
              </div>
            )}
          </div>

          {/* Mobile menu */}
          <Sheet open={open} onOpenChange={setOpen}>
            <SheetTrigger asChild className="lg:hidden">
              <Button variant="ghost" size="sm" className="h-9 w-9 p-0">
                <Menu className="h-4 w-4" />
                <span className="sr-only">Toggle menu</span>
              </Button>
            </SheetTrigger>
            <SheetContent side="right" className="w-80 pr-0">
              <SheetTitle className="sr-only">Navigation Menu</SheetTitle>
              <div className="flex flex-col h-full">
                {/* Mobile Logo */}
                <div className="flex items-center space-x-2 px-6 py-4 border-b">
                  <div className="bg-primary h-6 w-6 rounded flex items-center justify-center">
                    <span className="text-primary-foreground font-mono font-bold text-xs">
                      0
                    </span>
                  </div>
                  <span className="font-bold">the0</span>
                  <span className="px-1 py-0.5 text-[8px] font-medium bg-orange-100 text-orange-800 dark:bg-orange-900/30 dark:text-orange-300 rounded border border-orange-200 dark:border-orange-800">
                    BETA
                  </span>
                </div>

                <div className="flex-1 overflow-auto">
                  {/* Mobile Search */}
                  {/* Search functionality removed */}

                  {/* Mobile Navigation */}
                  <div className="px-6 py-4 space-y-4">
                    <div className="space-y-3">
                      <Link
                        href="/about"
                        className="block text-sm font-medium transition-colors hover:text-primary"
                        onClick={() => setOpen(false)}
                      >
                        About
                      </Link>
                      <Link
                        href={config.docsUrl}
                        className="block text-sm font-medium transition-colors hover:text-primary"
                        onClick={() => setOpen(false)}
                        target="_blank"
                        rel="noopener noreferrer"
                      >
                        Docs
                      </Link>
                    </div>
                  </div>
                </div>

                {/* Mobile Auth */}
                <div className="border-t p-6 space-y-4">
                  <div className="flex items-center justify-between">
                    <span className="text-sm text-muted-foreground">Theme</span>
                    <ModeToggle />
                  </div>

                  {loading ? (
                    <div className="space-y-2">
                      <Skeleton className="h-9 w-full" />
                      <Skeleton className="h-9 w-full" />
                    </div>
                  ) : user ? (
                    <div className="space-y-2">
                      <div className="flex items-center space-x-2 p-2 rounded-md bg-muted">
                        <User2 className="h-4 w-4" />
                        <span className="text-sm font-medium">
                          {user.username || "User"}
                        </span>
                      </div>
                      <Button
                        asChild
                        className="w-full"
                        onClick={() => setOpen(false)}
                      >
                        <Link href="/dashboard">Dashboard</Link>
                      </Button>
                    </div>
                  ) : (
                    <div className="space-y-2">
                      <Button
                        variant="outline"
                        asChild
                        className="w-full"
                        onClick={() => setOpen(false)}
                      >
                        <Link href="/login">Log in</Link>
                      </Button>
                      <Button
                        asChild
                        className="w-full"
                        onClick={() => setOpen(false)}
                      >
                        <Link href="/register">Sign up</Link>
                      </Button>
                    </div>
                  )}
                </div>
              </div>
            </SheetContent>
          </Sheet>
        </div>
      </div>
    </header>
  );
}
