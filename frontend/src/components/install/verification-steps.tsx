'use client';

import { useState } from 'react';
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { CodeBlock } from './quick-copy-section';
import {
  CheckCircle,
  Circle,
  Terminal,
  ExternalLink,
  HelpCircle,
} from 'lucide-react';
import type { VerificationStepsProps, VerificationStep } from '@/types/install';

export function VerificationSteps({ platform }: VerificationStepsProps) {
  const [completedSteps, setCompletedSteps] = useState<Set<string>>(new Set());

  const steps = getVerificationSteps(platform);

  const toggleStep = (stepId: string) => {
    const newCompleted = new Set(completedSteps);
    if (newCompleted.has(stepId)) {
      newCompleted.delete(stepId);
    } else {
      newCompleted.add(stepId);
    }
    setCompletedSteps(newCompleted);
  };

  const allStepsCompleted =
    steps.length > 0 && steps.every((step) => completedSteps.has(step.id));
  const completedCount = steps.filter((step) =>
    completedSteps.has(step.id),
  ).length;

  return (
    <Card>
      <CardHeader>
        <div className="flex items-center justify-between">
          <div>
            <CardTitle className="flex items-center space-x-2">
              <Terminal className="h-5 w-5" />
              <span>Verify Installation</span>
            </CardTitle>
            <CardDescription>
              Follow these steps to confirm THE0 CLI was installed correctly and
              is ready to use.
            </CardDescription>
          </div>
          <Badge variant={allStepsCompleted ? 'default' : 'secondary'}>
            {completedCount}/{steps.length} completed
          </Badge>
        </div>
      </CardHeader>

      <CardContent className="space-y-6">
        {steps.length === 0 ? (
          <Alert>
            <HelpCircle className="h-4 w-4" />
            <AlertDescription>
              No platform specified. Select your platform above to see
              verification steps.
            </AlertDescription>
          </Alert>
        ) : (
          <>
            <div className="space-y-4">
              {steps.map((step, index) => (
                <VerificationStepItem
                  key={step.id}
                  step={step}
                  stepNumber={index + 1}
                  isCompleted={completedSteps.has(step.id)}
                  onToggle={() => toggleStep(step.id)}
                />
              ))}
            </div>

            {allStepsCompleted && (
              <Alert className="border-green-200 dark:border-green-800 bg-green-50 dark:bg-green-950/30">
                <CheckCircle className="h-4 w-4 text-green-600 dark:text-green-400" />
                <AlertDescription className="text-green-800 dark:text-green-200">
                  <strong>Great!</strong> THE0 CLI is installed and ready to
                  use. You can now start managing your trading bots from the
                  command line.
                </AlertDescription>
              </Alert>
            )}

            <div className="pt-4 border-t space-y-3">
              <h4 className="font-medium">Next Steps</h4>
              <div className="space-y-2 text-sm">
                <div className="flex items-center space-x-2">
                  <span>1. Authenticate with your THE0 account:</span>
                  <code className="bg-muted px-2 py-1 rounded text-xs">
                    the0 auth login
                  </code>
                </div>
                <div className="flex items-center space-x-2">
                  <span>2. View available commands:</span>
                  <code className="bg-muted px-2 py-1 rounded text-xs">
                    the0 --help
                  </code>
                </div>
                <div className="flex items-center space-x-2">
                  <span>3. List your bots:</span>
                  <code className="bg-muted px-2 py-1 rounded text-xs">
                    the0 bot list
                  </code>
                </div>
              </div>

              <Button variant="outline" size="sm" asChild>
                <a
                  href="https://docs.theo.ai/cli"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  <ExternalLink className="h-4 w-4 mr-2" />
                  View CLI Documentation
                </a>
              </Button>
            </div>
          </>
        )}
      </CardContent>
    </Card>
  );
}

// Individual verification step component
function VerificationStepItem({
  step,
  stepNumber,
  isCompleted,
  onToggle,
}: {
  step: VerificationStep;
  stepNumber: number;
  isCompleted: boolean;
  onToggle: () => void;
}) {
  return (
    <div className="space-y-3">
      <div className="flex items-start space-x-3">
        <button
          onClick={onToggle}
          className="flex-shrink-0 mt-1"
          aria-label={`Mark step ${stepNumber} as ${isCompleted ? 'incomplete' : 'complete'}`}
        >
          {isCompleted ? (
            <CheckCircle className="h-5 w-5 text-green-600" />
          ) : (
            <Circle className="h-5 w-5 text-muted-foreground" />
          )}
        </button>

        <div className="flex-1 space-y-2">
          <div className="flex items-center justify-between">
            <h4
              className={`font-medium ${isCompleted ? 'text-green-600' : ''}`}
            >
              {stepNumber}. {step.title}
            </h4>
          </div>

          <p className="text-sm text-muted-foreground">{step.description}</p>

          <CodeBlock code={step.command} showCopy={true} />

          {step.expectedOutput && (
            <div className="space-y-1">
              <p className="text-xs font-medium text-muted-foreground">
                Expected output:
              </p>
              <div className="bg-green-50 dark:bg-green-950/30 border border-green-200 dark:border-green-800 rounded p-3 overflow-x-auto">
                <code className="text-xs text-green-800 dark:text-green-200 whitespace-pre-wrap break-all">
                  {step.expectedOutput}
                </code>
              </div>
            </div>
          )}
        </div>
      </div>
    </div>
  );
}

// Compact verification component for smaller spaces
export function CompactVerificationSteps({ platform }: VerificationStepsProps) {
  const steps = getVerificationSteps(platform);
  const [currentStep, setCurrentStep] = useState(0);

  if (steps.length === 0) {
    return null;
  }

  const step = steps[currentStep];

  return (
    <Card>
      <CardHeader className="pb-3">
        <div className="flex items-center justify-between">
          <CardTitle className="text-base">Verify Installation</CardTitle>
          <Badge variant="secondary">
            {currentStep + 1}/{steps.length}
          </Badge>
        </div>
      </CardHeader>

      <CardContent className="space-y-3">
        <div>
          <h4 className="font-medium">{step.title}</h4>
          <p className="text-sm text-muted-foreground">{step.description}</p>
        </div>

        <CodeBlock code={step.command} showCopy={true} />

        <div className="flex justify-between">
          <Button
            variant="outline"
            size="sm"
            onClick={() => setCurrentStep(Math.max(0, currentStep - 1))}
            disabled={currentStep === 0}
          >
            Previous
          </Button>
          <Button
            variant="outline"
            size="sm"
            onClick={() =>
              setCurrentStep(Math.min(steps.length - 1, currentStep + 1))
            }
            disabled={currentStep === steps.length - 1}
          >
            Next
          </Button>
        </div>
      </CardContent>
    </Card>
  );
}

// Get verification steps based on platform
function getVerificationSteps(platform?: string): VerificationStep[] {
  const isWindows = platform?.toLowerCase().includes('windows');
  const binaryName = isWindows ? 'the0.exe' : 'the0';

  const baseSteps: VerificationStep[] = [
    {
      id: 'check-version',
      title: 'Check CLI Version',
      description:
        'Verify that THE0 CLI is installed and accessible in your PATH.',
      command: `${binaryName} --version`,
      expectedOutput: 'the0 version 1.0.0',
    },
    {
      id: 'check-help',
      title: 'View Help Information',
      description:
        'Confirm that the CLI is working and see available commands.',
      command: `${binaryName} --help`,
      expectedOutput:
        'THE0 CLI - Manage your trading bots from the command line',
    },
    {
      id: 'check-auth-status',
      title: 'Check Authentication Status',
      description: 'See if you need to authenticate with your THE0 account.',
      command: `${binaryName} auth status`,
      expectedOutput: 'Not authenticated. Run "the0 auth login" to sign in.',
    },
  ];

  // Add platform-specific steps
  if (isWindows) {
    baseSteps.push({
      id: 'check-path-windows',
      title: 'Verify PATH Configuration',
      description: 'Ensure THE0 CLI is available in new PowerShell sessions.',
      command: 'Get-Command the0',
      expectedOutput: 'C:\\Users\\username\\bin\\the0.exe',
    });
  } else {
    baseSteps.push({
      id: 'check-permissions',
      title: 'Verify File Permissions',
      description: 'Check that the binary has correct executable permissions.',
      command: `which ${binaryName} && ls -la $(which ${binaryName})`,
      expectedOutput: '-rwxr-xr-x 1 user staff 1234567 /home/user/bin/the0',
    });
  }

  return baseSteps;
}

// Hook for verification state management
export function useVerificationState(platform?: string) {
  const [completedSteps, setCompletedSteps] = useState<Set<string>>(new Set());
  const steps = getVerificationSteps(platform);

  const toggleStep = (stepId: string) => {
    const newCompleted = new Set(completedSteps);
    if (newCompleted.has(stepId)) {
      newCompleted.delete(stepId);
    } else {
      newCompleted.add(stepId);
    }
    setCompletedSteps(newCompleted);
  };

  const resetSteps = () => {
    setCompletedSteps(new Set());
  };

  const allStepsCompleted =
    steps.length > 0 && steps.every((step) => completedSteps.has(step.id));
  const completedCount = steps.filter((step) =>
    completedSteps.has(step.id),
  ).length;
  const progress = steps.length > 0 ? (completedCount / steps.length) * 100 : 0;

  return {
    steps,
    completedSteps,
    toggleStep,
    resetSteps,
    allStepsCompleted,
    completedCount,
    progress,
  };
}
