import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import {
  Brain,
  AlertTriangle,
  CheckCircle,
  XCircle,
  Info,
  Zap,
} from "lucide-react";
import { Badge } from "@/components/ui/badge";
import { Alert, AlertDescription } from "@/components/ui/alert";
import { Progress } from "@/components/ui/progress";
import { Separator } from "@/components/ui/separator";
import React from "react";
import ReactMarkdown from "react-markdown";
import { MarkdownComponents } from "./markdown-components";

interface AIReviewSectionProps {
  review: any;
  status: string;
}

export const AIReviewSection = ({ review, status }: AIReviewSectionProps) => {
  const aiAnalysis = review?.aiAnalysis;

  // If no AI analysis data is available
  if (!aiAnalysis) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Brain className="h-5 w-5" />
            🤖 AI Security Review
          </CardTitle>
        </CardHeader>
        <CardContent>
          <Alert>
            <Info className="h-4 w-4" />
            <AlertDescription>
              AI review is not available for this bot version. The security
              analysis was performed using traditional rule-based scanning only.
            </AlertDescription>
          </Alert>
        </CardContent>
      </Card>
    );
  }

  // If AI analysis is disabled
  if (!aiAnalysis.enabled) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Brain className="h-5 w-5 opacity-50" />
            🤖 AI Security Review
          </CardTitle>
        </CardHeader>
        <CardContent>
          <Alert>
            <Info className="h-4 w-4" />
            <AlertDescription>
              AI analysis was not performed for this bot. This may occur when
              traditional security scanning detects critical issues that require
              immediate attention.
            </AlertDescription>
          </Alert>
        </CardContent>
      </Card>
    );
  }

  // If AI analysis failed
  if (aiAnalysis.error) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Brain className="h-5 w-5 text-red-500" />
            🤖 AI Security Review
          </CardTitle>
        </CardHeader>
        <CardContent>
          <Alert variant="destructive">
            <XCircle className="h-4 w-4" />
            <AlertDescription>
              AI analysis encountered an error: {aiAnalysis.error}
            </AlertDescription>
          </Alert>
        </CardContent>
      </Card>
    );
  }

  // Calculate AI security score for display (0-5 scale to 0-100)
  const aiSecurityScore = Math.max(0, 100 - (aiAnalysis.score || 0) * 20);
  const confidencePercentage = Math.round((aiAnalysis.confidence || 0) * 100);

  // Determine AI threat level based on score
  const getAIThreatLevel = (score: number) => {
    if (score === 0) return "none";
    if (score <= 1) return "low";
    if (score <= 2) return "medium";
    if (score <= 3) return "high";
    return "critical";
  };

  const aiThreatLevel = getAIThreatLevel(aiAnalysis.score || 0);

  // Threat level colors
  const getThreatColor = (level: string) => {
    switch (level) {
      case "none":
        return "text-green-600 dark:text-green-400";
      case "low":
        return "text-blue-600 dark:text-blue-400";
      case "medium":
        return "text-yellow-600 dark:text-yellow-400";
      case "high":
        return "text-orange-600 dark:text-orange-400";
      case "critical":
        return "text-red-600 dark:text-red-400";
      default:
        return "text-gray-600 dark:text-gray-400";
    }
  };

  const getConfidenceColor = (confidence: number) => {
    if (confidence >= 80) return "text-green-600 dark:text-green-400";
    if (confidence >= 60) return "text-yellow-600 dark:text-yellow-400";
    return "text-red-600 dark:text-red-400";
  };

  const getConfidenceIcon = (confidence: number) => {
    if (confidence >= 80)
      return (
        <CheckCircle className="h-4 w-4 text-green-600 dark:text-green-400" />
      );
    if (confidence >= 60)
      return (
        <AlertTriangle className="h-4 w-4 text-yellow-600 dark:text-yellow-400" />
      );
    return <XCircle className="h-4 w-4 text-red-600 dark:text-red-400" />;
  };

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <Brain className="h-5 w-5" />
          🤖 AI Security Review
        </CardTitle>
      </CardHeader>
      <CardContent className="space-y-6">
        {/* AI Security Score */}
        <div className="space-y-3">
          <div className="flex items-center justify-between">
            <span className="text-sm font-medium">AI Security Score</span>
            <span className={`font-bold ${getThreatColor(aiThreatLevel)}`}>
              {aiSecurityScore}/100
            </span>
          </div>
          <Progress value={aiSecurityScore} className="h-2" />
          <div className="flex items-center justify-between text-sm text-muted-foreground">
            <span>AI Threat Assessment:</span>
            <Badge
              variant={
                aiThreatLevel === "none" || aiThreatLevel === "low"
                  ? "default"
                  : "destructive"
              }
              className={getThreatColor(aiThreatLevel)}
            >
              {aiThreatLevel.toUpperCase()}
            </Badge>
          </div>
        </div>

        <Separator />

        {/* AI Confidence and Analysis Details */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div className="text-center p-4 bg-muted/30 rounded-lg">
            <div className="flex items-center justify-center h-8 w-8 rounded-full bg-primary/10 mx-auto mb-2">
              <Zap className="h-4 w-4 text-primary" />
            </div>
            <div
              className={`text-2xl font-bold ${getConfidenceColor(confidencePercentage)}`}
            >
              {confidencePercentage}%
            </div>
            <div className="text-sm text-muted-foreground">AI Confidence</div>
          </div>
          <div className="text-center p-4 bg-muted/30 rounded-lg">
            <div className="flex items-center justify-center h-8 w-8 rounded-full bg-blue-100 dark:bg-blue-900/20 mx-auto mb-2">
              {getConfidenceIcon(confidencePercentage)}
            </div>
            <div className="text-2xl font-bold">{aiAnalysis.score || 0}/5</div>
            <div className="text-sm text-muted-foreground">Risk Score</div>
          </div>
        </div>

        <Separator />

        {/* AI Analysis Verdict */}
        <Alert
          variant={
            aiThreatLevel === "none" || aiThreatLevel === "low"
              ? "default"
              : "destructive"
          }
        >
          <div className="flex items-start gap-3">
            <div className="text-2xl">🤖</div>
            <div className="flex-1">
              <div className="font-medium mb-1">AI Analysis Complete</div>
              <AlertDescription className="text-sm prose prose-sm max-w-none">
                <ReactMarkdown components={MarkdownComponents}>
                  {aiAnalysis.reason ||
                    "AI security analysis completed successfully"}
                </ReactMarkdown>
              </AlertDescription>
              <div className="text-xs text-muted-foreground mt-2 space-y-1">
                <div>Confidence Level: {confidencePercentage}%</div>
                <div>Risk Assessment: {aiAnalysis.score || 0} out of 5</div>
              </div>
            </div>
          </div>
        </Alert>

        {/* Confidence Level Explanation */}
        <div className="text-xs text-muted-foreground bg-muted/30 p-3 rounded-lg">
          <div className="font-medium mb-1">Understanding AI Confidence:</div>
          <div className="space-y-1">
            <div>
              • <strong>High (80%+):</strong> AI is very confident in its
              assessment
            </div>
            <div>
              • <strong>Medium (60-79%):</strong> AI assessment should be
              reviewed
            </div>
            <div>
              • <strong>Low (&lt;60%):</strong> AI recommendation requires human
              verification
            </div>
          </div>
        </div>
      </CardContent>
    </Card>
  );
};
