import { Badge } from '@/components/ui/badge';
import React from 'react';
import { THREAT_COLORS } from '@/components/custom-bots/constants';

export const ReviewSummary = ({ review, status }: any) => {
  if (!review || status === 'pending_review') return null;

  const threatColor =
    THREAT_COLORS[review.threatSummary.threatLevel] || THREAT_COLORS.medium;

  return (
    <div className="mt-3 p-3 bg-muted/50 rounded-lg border">
      <div className="flex items-center justify-between mb-2">
        <div className="flex items-center gap-2">
          <span className="text-lg">{review.overseerBadge}</span>
          <span className="font-medium text-sm">0vers33r Security Report</span>
          <Badge
            variant="outline"
            className={`text-xs ${threatColor} border-current`}
          >
            {review.threatSummary.threatLevel}
          </Badge>
        </div>
        <span className="text-xs text-muted-foreground">
          {review.reviewedAt.toLocaleDateString()}
        </span>
      </div>

      <p className="text-sm text-foreground mb-2">{review.reason}</p>

      {review.issues && review.issues.length > 0 && (
        <div className="text-xs">
          <span className="font-medium text-destructive">Issues Found:</span>
          <div className="mt-1 flex flex-wrap gap-1">
            {review.issues.map((issue: any, index: any) => (
              <Badge key={index} variant="destructive" className="text-xs">
                {issue}
              </Badge>
            ))}
          </div>
        </div>
      )}

      {review.scannedFiles && (
        <div className="mt-2 text-xs text-muted-foreground">
          <span className="font-medium">Scanned files:</span>{' '}
          {review.scannedFiles.length} files analyzed
        </div>
      )}
    </div>
  );
};
