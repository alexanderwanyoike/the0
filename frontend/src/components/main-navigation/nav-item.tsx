import Link from "next/link";
import { cn } from "@/lib/utils";
import {
  Tooltip,
  TooltipContent,
  TooltipProvider,
  TooltipTrigger,
} from "@/components/ui/tooltip";
import { JSX } from "react";

interface NavItemProps {
  href: string;
  icon: (props: React.SVGProps<SVGSVGElement>) => JSX.Element;
  name: string;
  isActive: boolean;
  isCollapsed: boolean;
}

export function NavItem({
  href,
  icon: Icon,
  name,
  isActive,
  isCollapsed,
}: NavItemProps) {
  const link = (
    <Link
      href={href}
      className={cn(
        "flex items-center rounded-lg text-sm font-medium transition-colors",
        "hover:bg-accent hover:text-accent-foreground mb-2",
        isCollapsed ? "justify-center p-2" : "px-3 py-2",
        isActive ? "bg-accent text-accent-foreground" : "text-muted-foreground",
      )}
    >
      <Icon
        className={cn("h-4 w-4", !isCollapsed && "mr-3")}
        aria-hidden="true"
      />
      {!isCollapsed && <span>{name}</span>}
    </Link>
  );

  if (isCollapsed) {
    return (
      <TooltipProvider>
        <Tooltip>
          <TooltipTrigger asChild>{link}</TooltipTrigger>
          <TooltipContent side="right" sideOffset={10}>
            {name}
          </TooltipContent>
        </Tooltip>
      </TooltipProvider>
    );
  }

  return link;
}
