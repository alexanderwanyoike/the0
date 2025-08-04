import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from '@/components/ui/card';
import { Activity, FileText } from 'lucide-react';
import { Progress } from '@/components/ui/progress';
import React from 'react';

export const ScanProgress = ({ review, status }: any) => {
  if (status !== 'pending_review' || !review?.scanProgress) return null;

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <Activity className="h-5 w-5" />
          Real-time Scan Progress
        </CardTitle>
        <CardDescription>
          0vers33r is analyzing your bot code for security vulnerabilities
        </CardDescription>
      </CardHeader>
      <CardContent className="space-y-4">
        <div className="space-y-2">
          <div className="flex justify-between text-sm">
            <span>Scanning Progress</span>
            <span>{review.scanProgress}%</span>
          </div>
          <Progress value={review.scanProgress} className="h-2" />
        </div>

        {review.scanDetails && (
          <div className="grid grid-cols-2 gap-4 text-sm">
            <div className="space-y-2">
              <div className="flex justify-between">
                <span className="text-muted-foreground">Files Scanned:</span>
                <span className="font-medium">
                  {review.scanDetails.scannedFiles}/
                  {review.scanDetails.totalFiles}
                </span>
              </div>
              <div className="flex justify-between">
                <span className="text-muted-foreground">Clean Files:</span>
                <span className="font-medium text-green-600">
                  {review.scanDetails.cleanFiles}
                </span>
              </div>
            </div>
            <div className="space-y-2">
              <div className="flex justify-between">
                <span className="text-muted-foreground">
                  Suspicious Patterns:
                </span>
                <span className="font-medium text-yellow-600">
                  {review.scanDetails.suspiciousPatterns}
                </span>
              </div>
              <div className="flex justify-between">
                <span className="text-muted-foreground">
                  Dangerous Patterns:
                </span>
                <span className="font-medium text-red-600">
                  {review.scanDetails.dangerousPatterns}
                </span>
              </div>
            </div>
          </div>
        )}

        {review.scannedFiles && (
          <div className="space-y-2">
            <h4 className="font-medium text-sm">File Analysis:</h4>
            <div className="bg-muted/50 rounded-lg p-3 font-mono text-xs space-y-1">
              {review.scannedFiles.map((file: string, index: any) => (
                <div key={index} className="flex items-center gap-2">
                  <FileText className="h-3 w-3 text-muted-foreground" />
                  <span>{file}</span>
                </div>
              ))}
            </div>
          </div>
        )}
      </CardContent>
    </Card>
  );
};
