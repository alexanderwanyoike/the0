import { CheckCircle } from "lucide-react";

export const STATUS_CONFIG: any = {
  active: {
    color:
      "bg-green-100 text-green-800 border-green-200 dark:bg-green-900/20 dark:text-green-300 dark:border-green-800",
    icon: CheckCircle,
    text: "Active",
    description: "Bot is ready for deployment",
  },
};

export const BOT_TYPE_COLORS: any = {
  "real-time":
    "bg-blue-100 text-blue-800 dark:bg-blue-900/20 dark:text-blue-300",
  scheduled:
    "bg-purple-100 text-purple-800 dark:bg-purple-900/20 dark:text-purple-300",
  "event-driven":
    "bg-emerald-100 text-emerald-800 dark:bg-emerald-900/20 dark:text-emerald-300",
};
