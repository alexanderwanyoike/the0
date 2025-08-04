import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from '@/components/ui/card';
import {
  Bot,
  Calendar,
  ChevronRight,
  Clock,
  Eye,
  GitBranch,
  Share2,
  User,
} from 'lucide-react';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import React from 'react';
import {
  BOT_TYPE_COLORS,
  STATUS_CONFIG,
  THREAT_COLORS,
} from '@/components/custom-bots/constants';
import { StatusBadge } from '@/components/custom-bots/status-badge';

export const BotCard = ({ bot, onClick }: any) => {
  // Get latest version data since CustomBotWithVersions doesn't have direct status/config
  const latestVersion = bot.versions?.[0]; // Versions are sorted by creation date desc
  const botStatus = latestVersion?.status || 'pending_review';
  const botConfig = latestVersion?.config;

  const config = STATUS_CONFIG[botStatus] || {
    color: 'bg-gray-100 text-gray-800 border-gray-200',
    icon: Clock,
    text: 'Unknown Status',
    description: 'Status not recognized'
  };

  // Get security info for compact display
  const hasSecurityIssues = latestVersion?.review?.issues && latestVersion.review.issues.length > 0;
  const issueCount = latestVersion?.review?.issues?.length || 0;

  return (
    <Card
      className="group hover:shadow-lg hover:shadow-primary/5 transition-all duration-200 cursor-pointer border-0 bg-gradient-to-br from-background to-muted/20 hover:from-background hover:to-primary/5"
      onClick={onClick}
    >
      <CardHeader className="pb-4 space-y-0">
        <div className="flex items-center justify-between mb-3">
          <div className="flex items-center gap-3">
            <div className="relative">
              <div className="flex items-center justify-center h-12 w-12 rounded-xl bg-gradient-to-br from-primary/10 to-primary/20 group-hover:from-primary/20 group-hover:to-primary/30 transition-colors">
                <Bot className="h-6 w-6 text-primary" />
              </div>
              {/* Status indicator dot */}
              <div
                className={`absolute -top-1 -right-1 h-4 w-4 rounded-full border-2 border-background ${config.color.includes('green') ? 'bg-green-500' : config.color.includes('red') ? 'bg-red-500' : config.color.includes('yellow') ? 'bg-yellow-500' : 'bg-orange-500'}`}
              />
            </div>
            <div className="flex-1 min-w-0">
              <CardTitle className="text-xl font-semibold truncate mb-1">
                {bot.name}
              </CardTitle>
              <div className="flex items-center gap-2">
                <Badge
                  variant="secondary"
                  className={`text-xs px-2 py-1 font-medium ${BOT_TYPE_COLORS[botConfig?.type] || 'bg-gray-100 text-gray-800'}`}
                >
                  {botConfig?.type || 'unknown'}
                </Badge>
                <span className="text-sm text-muted-foreground font-mono">
                  v{bot.latestVersion}
                </span>
              </div>
            </div>
          </div>
          <ChevronRight className="h-5 w-5 text-muted-foreground group-hover:text-primary transition-colors" />
        </div>

        {/* Status Badge */}
        <div className="flex items-center justify-between">
          <StatusBadge status={botStatus} review={latestVersion?.review} />
          {hasSecurityIssues && (
            <Badge variant="destructive" className="text-xs">
              {issueCount} issue{issueCount !== 1 ? 's' : ''}
            </Badge>
          )}
        </div>
      </CardHeader>

      <CardContent className="pt-0 space-y-4">
        {/* Description */}
        <CardDescription className="text-sm leading-relaxed line-clamp-2 text-foreground/70">
          {botConfig?.description || 'No description available'}
        </CardDescription>

        {/* Metadata */}
        <div className="flex items-center justify-between text-xs text-muted-foreground">
          <div className="flex items-center gap-1">
            <Calendar className="w-3 h-3" />
            <span>{bot.createdAt.toLocaleDateString()}</span>
          </div>
          <div className="flex items-center gap-1">
            <GitBranch className="w-3 h-3" />
            <span>
              {bot.versions.length} version
              {bot.versions.length !== 1 ? 's' : ''}
            </span>
          </div>
          <div className="flex items-center gap-1">
            <User className="w-3 h-3" />
            <span className="truncate max-w-[80px]">{botConfig?.author || 'Unknown'}</span>
          </div>
        </div>

        {/* Security Summary - Compact */}
        {latestVersion?.review && botStatus !== 'pending_review' && (
          <div className="flex items-center justify-between p-3 bg-muted/30 rounded-lg border">
            <div className="flex items-center gap-2">
              <span className="text-sm">🕵️</span>
              <span className="text-sm font-medium">0vers33r</span>
              <Badge
                variant="outline"
                className={`text-xs ${THREAT_COLORS[latestVersion.review.threatSummary?.threatLevel] || THREAT_COLORS.medium} border-current`}
              >
                {latestVersion.review.threatSummary?.threatLevel || 'low'}
              </Badge>
            </div>
            <span className="text-xs text-muted-foreground">
              {latestVersion.review.reviewedAt?.toLocaleDateString()}
            </span>
          </div>
        )}

        {/* Action Buttons */}
        <div className="flex gap-2 pt-2">
          <Button
            size="sm"
            variant="outline"
            className="flex-1 h-9 border-muted-foreground/20 hover:border-primary hover:bg-primary/5"
            onClick={(e) => {
              e.stopPropagation();
              onClick();
            }}
          >
            <Eye className="w-4 h-4 mr-2" />
            Details
          </Button>
        </div>
      </CardContent>
    </Card>
  );
};
