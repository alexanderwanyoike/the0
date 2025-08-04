import { AlertTriangle, CheckCircle, Clock, XCircle } from 'lucide-react';

export const STATUS_CONFIG: any = {
  pending_review: {
    color:
      'bg-yellow-100 text-yellow-800 border-yellow-200 dark:bg-yellow-900/20 dark:text-yellow-300 dark:border-yellow-800',
    icon: Clock,
    text: 'Scanning in Progress',
    description: '0vers33r is analyzing your bot code for security threats',
  },
  approved: {
    color:
      'bg-green-100 text-green-800 border-green-200 dark:bg-green-900/20 dark:text-green-300 dark:border-green-800',
    icon: CheckCircle,
    text: 'Security Approved',
    description: 'Bot passed all security checks and is ready for deployment',
  },
  declined: {
    color:
      'bg-red-100 text-red-800 border-red-200 dark:bg-red-900/20 dark:text-red-300 dark:border-red-800',
    icon: XCircle,
    text: 'Security Threat Detected',
    description: 'Dangerous code patterns found - bot cannot be deployed',
  },
  awaiting_human_review: {
    color:
      'bg-orange-100 text-orange-800 border-orange-200 dark:bg-orange-900/20 dark:text-orange-300 dark:border-orange-800',
    icon: AlertTriangle,
    text: 'Awaiting Human Review',
    description: 'Suspicious patterns detected - manual review required',
  },
  published: {
    color:
      'bg-blue-100 text-blue-800 border-blue-200 dark:bg-blue-900/20 dark:text-blue-300 dark:border-blue-800',
    icon: CheckCircle,
    text: 'Published',
    description: 'Bot is live and available for users to deploy',
  },
};

export const BOT_TYPE_COLORS: any = {
  'real-time':
    'bg-blue-100 text-blue-800 dark:bg-blue-900/20 dark:text-blue-300',
  scheduled:
    'bg-purple-100 text-purple-800 dark:bg-purple-900/20 dark:text-purple-300',
  'event-driven':
    'bg-emerald-100 text-emerald-800 dark:bg-emerald-900/20 dark:text-emerald-300',
};

export const THREAT_COLORS: any = {
  none: 'text-green-600 dark:text-green-400',
  low: 'text-green-600 dark:text-green-400',
  medium: 'text-yellow-600 dark:text-yellow-400',
  high: 'text-red-600 dark:text-red-400',
  critical: 'text-red-700 dark:text-red-300',
};
