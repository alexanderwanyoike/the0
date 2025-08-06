"use client";

import * as React from "react";
import Link from "next/link";
import { Button } from "@/components/ui/button";
import {
  Sheet,
  SheetContent,
  SheetTrigger,
  SheetTitle,
} from "@/components/ui/sheet";
import { ModeToggle } from "@/components/mode-toggle";
import { Menu, Github } from "lucide-react";

export function Navigation() {
  const [open, setOpen] = React.useState(false);

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
        </nav>

        {/* Right side items */}
        <div className="flex items-center space-x-2 ml-auto">
          {/* Desktop buttons */}
          <div className="hidden sm:flex sm:items-center sm:space-x-2">
            <ModeToggle />
            <Button variant="ghost" size="sm" asChild>
              <a
                href="https://github.com/alexanderwanyoike/the0"
                target="_blank"
                rel="noopener noreferrer"
                className="gap-2"
              >
                <Github className="h-4 w-4" />
                <span className="hidden lg:inline">GitHub</span>
              </a>
            </Button>
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
                    </div>
                  </div>
                </div>

                {/* Mobile footer */}
                <div className="border-t p-6 space-y-4">
                  <div className="flex items-center justify-between">
                    <span className="text-sm text-muted-foreground">Theme</span>
                    <ModeToggle />
                  </div>

                  <Button
                    variant="outline"
                    asChild
                    className="w-full gap-2"
                    onClick={() => setOpen(false)}
                  >
                    <a
                      href="https://github.com/alexanderwanyoike/the0"
                      target="_blank"
                      rel="noopener noreferrer"
                    >
                      <Github className="h-4 w-4" />
                      View on GitHub
                    </a>
                  </Button>
                </div>
              </div>
            </SheetContent>
          </Sheet>
        </div>
      </div>
    </header>
  );
}
