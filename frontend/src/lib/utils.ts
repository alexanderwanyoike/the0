import { clsx, type ClassValue } from "clsx";
import { twMerge } from "tailwind-merge";
import { CustomBotConfig } from "@/types/custom-bots";

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}

export function canBotBeBacktested(config: CustomBotConfig): boolean {
  return !!(config.entrypoints?.backtest && config.schema?.backtest);
}
