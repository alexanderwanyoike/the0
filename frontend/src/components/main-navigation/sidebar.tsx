import { AuthUser } from '@/lib/auth/types';
import { cn } from '@/lib/utils';
import { NavItem } from './nav-item';
import { UserActionsButton } from './user-actions-button';
import { APP_NAME } from '@/lib/constants';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Button } from '@/components/ui/button';
import { PanelLeftClose, PanelLeftOpen } from 'lucide-react';
import { JSX, useRef } from 'react';
import Link from 'next/link';

type NavItemType = {
  name: string;
  href: string;
  icon: (props: React.SVGProps<SVGSVGElement>) => JSX.Element;
};

interface SidebarProps {
  navigation: NavItemType[];
  user: AuthUser | null;
  onLogout: () => void;
  currentPath: string;
  isCollapsed: boolean;
  onCollapsedChange: (collapsed: boolean) => void;
}

export function Sidebar({
  navigation,
  user,
  onLogout,
  currentPath,
  isCollapsed,
  onCollapsedChange,
}: SidebarProps) {
  return (
    <div
      className={cn(
        'fixed inset-y-0 left-0 z-20',
        'flex flex-col border-r bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60',
        'transition-all duration-300 ease-in-out',
        isCollapsed ? 'w-[60px]' : 'w-64',
      )}
    >
      {/* Logo section */}

      <div
        className={cn(
          'flex h-12 items-center gap-2 border-b shrink-0',
          isCollapsed ? 'justify-center px-0' : 'px-4',
        )}
      >
        <Link
          href="/"
          className="flex items-center space-x-2 hover:opacity-80 transition-opacity"
        >
          <div className="flex h-8 w-8 items-center justify-center rounded bg-primary">
            <span className="font-mono text-lg font-bold text-primary-foreground">
              0
            </span>
          </div>
          {!isCollapsed && (
            <div className="flex items-center gap-2">
              <span className="font-mono text-lg font-bold">{APP_NAME}</span>
              <span className="px-1 py-0.5 text-[8px] font-medium bg-orange-100 text-orange-800 dark:bg-orange-900/30 dark:text-orange-300 rounded-full border border-orange-200 dark:border-orange-800">
                BETA
              </span>
            </div>
          )}
        </Link>
      </div>
      {/* Toggle button */}
      <Button
        variant="ghost"
        size="icon"
        className={cn(
          'absolute -right-4 top-12 h-8 w-8 rounded-full border bg-background',
          'hover:bg-accent hover:text-accent-foreground',
        )}
        onClick={() => onCollapsedChange(!isCollapsed)}
      >
        {isCollapsed ? (
          <PanelLeftOpen className="h-4 w-4" />
        ) : (
          <PanelLeftClose className="h-4 w-4" />
        )}
      </Button>

      {/* Navigation section */}
      <nav className={cn('py-3', isCollapsed ? 'px-2' : 'px-4')}>
        {navigation.map((item) => (
          <NavItem
            key={item.name}
            href={item.href}
            icon={item.icon}
            name={item.name}
            isActive={currentPath === item.href}
            isCollapsed={isCollapsed}
          />
        ))}
      </nav>
      {/* User section */}
      <div className="mt-auto border-t p-2">
        <UserActionsButton
          user={user}
          onLogout={onLogout}
          isCollapsed={isCollapsed}
        />
      </div>
    </div>
  );
}
