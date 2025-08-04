import React from 'react';
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
} from '@/components/ui/alert-dialog';
import { Badge } from '@/components/ui/badge';
import { Separator } from '@/components/ui/separator';
import { Card, CardContent } from '@/components/ui/card';
import {
  AlertTriangle,
  CheckCircle,
  Loader2,
  ArrowRight,
  Settings,
} from 'lucide-react';

interface BotUpdateConfirmationDialogProps {
  isOpen: boolean;
  onOpenChange: (open: boolean) => void;
  onConfirm: () => void;
  isUpdating: boolean;
  botName: string;
  oldConfig: Record<string, any>;
  newConfig: Record<string, any>;
}

const BotUpdateConfirmationDialog: React.FC<
  BotUpdateConfirmationDialogProps
> = ({
  isOpen,
  onOpenChange,
  onConfirm,
  isUpdating,
  botName,
  oldConfig,
  newConfig,
}) => {
  // Get configuration changes
  const getConfigChanges = () => {
    const changes: Array<{
      key: string;
      oldValue: any;
      newValue: any;
      type: 'added' | 'modified' | 'removed';
    }> = [];

    const allKeys = new Set([
      ...Object.keys(oldConfig || {}),
      ...Object.keys(newConfig || {}),
    ]);

    allKeys.forEach((key) => {
      const oldValue = oldConfig?.[key];
      const newValue = newConfig?.[key];

      if (oldValue === undefined && newValue !== undefined) {
        changes.push({ key, oldValue, newValue, type: 'added' });
      } else if (oldValue !== undefined && newValue === undefined) {
        changes.push({ key, oldValue, newValue, type: 'removed' });
      } else if (oldValue !== newValue) {
        changes.push({ key, oldValue, newValue, type: 'modified' });
      }
    });

    return changes;
  };

  // Format value for display
  const formatValue = (value: any): string => {
    if (value === null || value === undefined) {
      return 'null';
    }
    if (typeof value === 'boolean') {
      return value.toString();
    }
    if (typeof value === 'object') {
      return JSON.stringify(value);
    }
    return String(value);
  };

  // Check if there are any potentially destructive changes
  const hasDestructiveChanges = () => {
    const changes = getConfigChanges();
    const destructiveKeys = [
      'api_key',
      'secret_key',
      'apiKey',
      'secretKey',
      'exchange',
      'symbol',
      'amount',
      'strategy',
    ];

    return changes.some(
      (change) =>
        destructiveKeys.some((key) =>
          change.key.toLowerCase().includes(key.toLowerCase()),
        ) &&
        (change.type === 'modified' || change.type === 'removed'),
    );
  };

  const changes = getConfigChanges();
  const isDestructive = hasDestructiveChanges();

  return (
    <AlertDialog open={isOpen} onOpenChange={onOpenChange}>
      <AlertDialogContent className="max-w-2xl max-h-[80vh] overflow-hidden flex flex-col">
        <AlertDialogHeader>
          <AlertDialogTitle className="flex items-center gap-2">
            <Settings className="h-5 w-5" />
            Confirm Bot Update
          </AlertDialogTitle>
          <AlertDialogDescription>
            Review the configuration changes for <strong>{botName}</strong>{' '}
            before applying them.
          </AlertDialogDescription>
        </AlertDialogHeader>

        <div className="flex-1 overflow-y-auto space-y-4">
          {/* Warning for destructive changes */}
          {isDestructive && (
            <Card className="border-orange-200 bg-orange-50 dark:bg-orange-950/20">
              <CardContent className="p-4">
                <div className="flex items-start gap-3">
                  <AlertTriangle className="h-5 w-5 text-orange-600 flex-shrink-0 mt-0.5" />
                  <div>
                    <p className="text-sm font-medium text-orange-800 dark:text-orange-200">
                      Potentially Destructive Changes Detected
                    </p>
                    <p className="text-xs text-orange-700 dark:text-orange-300 mt-1">
                      Some changes may affect critical bot settings like API
                      keys or trading parameters. Make sure these changes are
                      intentional.
                    </p>
                  </div>
                </div>
              </CardContent>
            </Card>
          )}

          {/* Changes Summary */}
          <div className="space-y-3">
            <div className="flex items-center justify-between">
              <h4 className="text-sm font-medium">Configuration Changes</h4>
              <Badge variant="outline">
                {changes.length} change{changes.length !== 1 ? 's' : ''}
              </Badge>
            </div>

            {changes.length === 0 ? (
              <Card>
                <CardContent className="p-4 text-center">
                  <CheckCircle className="h-8 w-8 text-green-500 mx-auto mb-2" />
                  <p className="text-sm text-muted-foreground">
                    No configuration changes detected
                  </p>
                </CardContent>
              </Card>
            ) : (
              <div className="space-y-2 max-h-60 overflow-y-auto">
                {changes.map((change, index) => (
                  <Card key={index} className="border-l-4 border-l-blue-500">
                    <CardContent className="p-3">
                      <div className="space-y-2">
                        {/* Change header */}
                        <div className="flex items-center justify-between">
                          <span className="text-sm font-medium font-mono">
                            {change.key}
                          </span>
                          <Badge
                            variant={
                              change.type === 'added'
                                ? 'default'
                                : change.type === 'removed'
                                  ? 'destructive'
                                  : 'secondary'
                            }
                            className="text-xs"
                          >
                            {change.type}
                          </Badge>
                        </div>

                        {/* Value comparison */}
                        <div className="flex items-center gap-2 text-xs">
                          <div className="flex-1 min-w-0">
                            <p className="text-muted-foreground">From:</p>
                            <code className="block p-2 bg-red-50 dark:bg-red-950/20 rounded border text-red-700 dark:text-red-300 break-all">
                              {formatValue(change.oldValue)}
                            </code>
                          </div>
                          <ArrowRight className="h-3 w-3 text-muted-foreground flex-shrink-0" />
                          <div className="flex-1 min-w-0">
                            <p className="text-muted-foreground">To:</p>
                            <code className="block p-2 bg-green-50 dark:bg-green-950/20 rounded border text-green-700 dark:text-green-300 break-all">
                              {formatValue(change.newValue)}
                            </code>
                          </div>
                        </div>
                      </div>
                    </CardContent>
                  </Card>
                ))}
              </div>
            )}
          </div>

          <Separator />

          {/* Impact notice */}
          <Card>
            <CardContent className="p-4">
              <div className="flex items-start gap-3">
                <AlertTriangle className="h-4 w-4 text-amber-500 flex-shrink-0 mt-0.5" />
                <div className="text-sm">
                  <p className="font-medium text-foreground mb-1">
                    Important Notice
                  </p>
                  <ul className="text-muted-foreground space-y-1 text-xs">
                    <li>
                      • The bot will be restarted with the new configuration
                    </li>
                    <li>• Any running trades or operations may be affected</li>
                    <li>• Changes take effect immediately upon confirmation</li>
                    <li>• You can revert changes later if needed</li>
                  </ul>
                </div>
              </div>
            </CardContent>
          </Card>
        </div>

        <AlertDialogFooter>
          <AlertDialogCancel disabled={isUpdating}>Cancel</AlertDialogCancel>
          <AlertDialogAction
            onClick={onConfirm}
            disabled={isUpdating}
            className={
              isDestructive ? 'bg-orange-600 hover:bg-orange-700' : undefined
            }
          >
            {isUpdating ? (
              <>
                <Loader2 className="h-4 w-4 mr-2 animate-spin" />
                Updating...
              </>
            ) : (
              <>
                <Settings className="h-4 w-4 mr-2" />
                {isDestructive ? 'Update Anyway' : 'Confirm Update'}
              </>
            )}
          </AlertDialogAction>
        </AlertDialogFooter>
      </AlertDialogContent>
    </AlertDialog>
  );
};

export default BotUpdateConfirmationDialog;
