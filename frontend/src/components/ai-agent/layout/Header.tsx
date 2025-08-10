import { Button } from "@/components/ui/button";
import { Moon, Sun, Settings } from "lucide-react";
import { useThemeStore } from "@/stores/ai-agent/themeStore";
import { useEffect, useState } from "react";
import { SettingsModal } from "@/components/ai-agent/settings/SettingsModal";

export function Header() {
  const { isDark, toggleTheme } = useThemeStore();
  const [settingsOpen, setSettingsOpen] = useState(false);
  const [mounted, setMounted] = useState(false);

  // Handle hydration
  useEffect(() => {
    setMounted(true);
  }, []);

  // Apply theme to document
  useEffect(() => {
    if (!mounted) return;

    if (isDark) {
      document.documentElement.classList.add("dark");
    } else {
      document.documentElement.classList.remove("dark");
    }
  }, [isDark, mounted]);

  return (
    <header className="border-b border-border bg-card px-6 py-3">
      <div className="flex items-center justify-between">
        <div className="flex items-center gap-3">
          <div className="flex items-center gap-2">
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
            <h1 className="text-xl font-semibold">AI Agent Workbench</h1>
          </div>
          <div className="text-sm text-muted-foreground">
            Build trading bots with AI assistance
          </div>
        </div>

        <div className="flex items-center gap-2">
          <Button variant="ghost" size="icon" onClick={toggleTheme}>
            <Sun className="h-4 w-4 rotate-0 scale-100 transition-all dark:-rotate-90 dark:scale-0" />
            <Moon className="absolute h-4 w-4 rotate-90 scale-0 transition-all dark:rotate-0 dark:scale-100" />
            <span className="sr-only">Toggle theme</span>
          </Button>
          <Button
            variant="ghost"
            size="icon"
            onClick={() => setSettingsOpen(true)}
          >
            <Settings className="h-4 w-4" />
            <span className="sr-only">Settings</span>
          </Button>
        </div>
      </div>

      <SettingsModal open={settingsOpen} onOpenChange={setSettingsOpen} />
    </header>
  );
}
