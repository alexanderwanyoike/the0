import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import {
  Shield,
  AlertTriangle,
  CheckCircle,
  FileText,
  Scan,
} from "lucide-react";
import { Badge } from "@/components/ui/badge";
import { Alert, AlertDescription } from "@/components/ui/alert";
import { Progress } from "@/components/ui/progress";
import { Separator } from "@/components/ui/separator";
import {
  THREAT_COLORS,
  STATUS_CONFIG,
} from "@/components/custom-bots/constants";
import React from "react";

export const SecurityReport = ({ review, status }: any) => {
  if (!review || status === "pending_review") {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Shield className="h-5 w-5" />
            Security Analysis
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8 text-muted-foreground">
            <Scan className="h-12 w-12 mx-auto mb-4 opacity-50 animate-pulse" />
            <p>🕵️ 0vers33r is analyzing your bot code...</p>
            <p className="text-sm mt-2">Security scan in progress</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  const threatLevel = review.threatSummary.threatLevel || "low";
  // Use correct field names from 0vers33r service
  const filesScanned = review.filesScanned || [];
  const totalFiles = review.totalFiles || 0;
  const issues = review.issues || [];

  // Calculate clean files: total files minus files with issues
  const cleanFiles = Math.max(0, totalFiles - issues.length);

  // Calculate security score for display
  const securityScore = Math.max(0, 100 - (review.score || 0) * 20);

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <Shield className="h-5 w-5" />
          🕵️ 0vers33r Security Analysis
        </CardTitle>
      </CardHeader>
      <CardContent className="space-y-6">
        {/* Overall Security Score */}
        <div className="space-y-3">
          <div className="flex items-center justify-between">
            <span className="text-sm font-medium">Security Score</span>
            <span className={`font-bold ${THREAT_COLORS[threatLevel]}`}>
              {securityScore}/100
            </span>
          </div>
          <Progress value={securityScore} className="h-2" />
          <div className="flex items-center justify-between text-sm text-muted-foreground">
            <span>Threat Level:</span>
            <Badge
              variant={status === "approved" ? "default" : "destructive"}
              className={THREAT_COLORS[threatLevel]}
            >
              {threatLevel.toUpperCase()}
            </Badge>
          </div>
        </div>

        <Separator />

        {/* Scan Summary */}
        <div className="grid grid-cols-2 gap-4">
          <div className="text-center p-4 bg-muted/30 rounded-lg">
            <div className="flex items-center justify-center h-8 w-8 rounded-full bg-primary/10 mx-auto mb-2">
              <FileText className="h-4 w-4 text-primary" />
            </div>
            <div className="text-2xl font-bold">{totalFiles}</div>
            <div className="text-sm text-muted-foreground">Files Scanned</div>
          </div>
          <div className="text-center p-4 bg-muted/30 rounded-lg">
            <div className="flex items-center justify-center h-8 w-8 rounded-full bg-green-100 dark:bg-green-900/20 mx-auto mb-2">
              <CheckCircle className="h-4 w-4 text-green-600 dark:text-green-400" />
            </div>
            <div className="text-2xl font-bold text-green-600 dark:text-green-400">
              {cleanFiles}
            </div>
            <div className="text-sm text-muted-foreground">Clean Files</div>
          </div>
        </div>

        <Separator />

        {/* 0vers33r Verdict */}
        <Alert variant={status === "approved" ? "default" : "destructive"}>
          <div className="flex items-start gap-3">
            <div className="text-2xl">🕵️</div>
            <div className="flex-1">
              <div className="font-medium mb-1">0vers33r Analysis Complete</div>
              <AlertDescription className="text-sm">
                {review.reason || "Security analysis completed"}
              </AlertDescription>
              {review.reviewedAt && (
                <div className="text-xs text-muted-foreground mt-2">
                  Reviewed: {new Date(review.reviewedAt).toLocaleString()}
                </div>
              )}
            </div>
          </div>
        </Alert>

        {/* Issues Found */}
        {issues.length > 0 && (
          <div className="space-y-3">
            <h4 className="font-medium flex items-center gap-2">
              <AlertTriangle className="h-4 w-4 text-yellow-600" />
              Security Issues Found ({issues.length})
            </h4>
            <div className="space-y-2">
              {issues.map((issue: string, index: number) => (
                <div
                  key={index}
                  className="flex items-start gap-2 p-3 bg-red-50 dark:bg-red-900/10 border border-red-200 dark:border-red-800 rounded-lg"
                >
                  <AlertTriangle className="h-4 w-4 text-red-600 dark:text-red-400 mt-0.5 flex-shrink-0" />
                  <div className="text-sm">
                    <code className="text-red-800 dark:text-red-200 font-mono">
                      {issue}
                    </code>
                  </div>
                </div>
              ))}
            </div>
          </div>
        )}

        {/* Scanned Files List */}
        {filesScanned.length > 0 && (
          <div className="space-y-3">
            <h4 className="font-medium flex items-center gap-2">
              <FileText className="h-4 w-4" />
              Scanned Files ({filesScanned.length})
            </h4>
            <div className="max-h-48 overflow-y-auto space-y-1">
              {filesScanned.map((file: string, index: number) => (
                <div
                  key={index}
                  className="flex items-center gap-2 p-2 bg-muted/30 rounded text-sm"
                >
                  <CheckCircle className="h-3 w-3 text-green-600 dark:text-green-400" />
                  <code className="text-muted-foreground font-mono">
                    {file}
                  </code>
                </div>
              ))}
            </div>
          </div>
        )}

        {/* Technical Details */}
        <div className="text-xs text-muted-foreground space-y-1">
          <div>Reviewed by: {review.reviewedBy || "Unknown"}</div>
          <div>Analysis Score: {review.score || 0}/5</div>
          {review.overseerBadge && <div>Overseer: {review.overseerBadge}</div>}
        </div>
      </CardContent>
    </Card>
  );
};
