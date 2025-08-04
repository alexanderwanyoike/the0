'use client';

import React, { useState, useEffect } from 'react';
import { useParams, useSearchParams, useRouter } from 'next/navigation';
import { withAuth } from '@/components/auth/with-auth';
import DashboardLayout from '@/components/layouts/dashboard-layout';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { ArrowLeft, Bot, Loader2, Save, Play } from 'lucide-react';
import { useAuth } from '@/contexts/auth-context';
import { useToast } from '@/hooks/use-toast';
import FormGenerator from '@/components/bots/form-generator';
import { ReadmeComponent } from '@/components/custom-bots/readme-component';
import BotUpdateConfirmationDialog from '@/components/bots/bot-update-confirmation-dialog';
import { CustomBotConfig } from '@/types/custom-bots';

interface BotDetails {
  config: CustomBotConfig;
  hasAccess: boolean;
}

interface DeployedBot {
  id: string;
  name: string;
  config: Record<string, any>;
  status: string;
}

function BotDeployPage() {
  const params = useParams();
  const searchParams = useSearchParams();
  const router = useRouter();
  const { user, token } = useAuth();
  const { toast } = useToast();

  const name = params?.name as string;
  const version = params?.version as string;
  const botId = searchParams?.get('botId'); // If present, we're updating an existing bot

  const [botDetails, setBotDetails] = useState<BotDetails | null>(null);
  const [existingBot, setExistingBot] = useState<DeployedBot | null>(null);
  const [formData, setFormData] = useState<Record<string, any>>({});
  const [loading, setLoading] = useState(true);
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [validationErrors, setValidationErrors] = useState<
    Record<string, string>
  >({});
  const [showUpdateDialog, setShowUpdateDialog] = useState(false);

  const isUpdate = Boolean(botId);

  const fetchBotDetails = React.useCallback(async () => {
    if (!user || !name || !version || !token) return;

    try {
      const response = await fetch(
        `/api/custom-bots/${encodeURIComponent(name)}/${encodeURIComponent(version)}`,
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        },
      );

      if (!response.ok) {
        throw new Error('Failed to fetch bot details');
      }

      const data = await response.json();
      if (data.success) {
        setBotDetails(data.data);
        // Initialize form with default values from schema
        if (data.data.config.schema?.bot) {
          const defaultValues: Record<string, any> = {};
          Object.entries(data.data.config.schema.bot.properties || {}).forEach(
            ([key, prop]: [string, any]) => {
              if (prop.default !== undefined) {
                defaultValues[key] = prop.default;
              }
            },
          );
          setFormData(defaultValues);
        }
      } else {
        throw new Error(data.message || 'Failed to fetch bot details');
      }
    } catch (err: any) {
      setError(err.message);
      toast({
        title: 'Error',
        description: `Failed to load bot details: ${err.message}`,
        variant: 'destructive',
      });
    } finally {
      setLoading(false);
    }
  }, [user, name, version, token, toast]);

  const fetchExistingBot = React.useCallback(async () => {
    if (!user || !botId || !token) {
      return;
    }

    try {
      const response = await fetch(`/api/bot/${botId}`, {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });

      if (!response.ok) {
        const errorText = await response.text();
        console.error('fetchExistingBot: Response not ok:', errorText);
        throw new Error('Failed to fetch existing bot');
      }

      const data = await response.json();

      // The API returns the bot data directly, not wrapped in a success/data structure
      setExistingBot(data);

      // Extract only user-configurable fields from existing bot config
      // Exclude metadata fields that are managed by the system but keep name and description
      const existingConfig = data.config || {};
      const { type, version, runtime, author, ...userConfig } = existingConfig;

      // Merge existing values with current form data (existing values take precedence)
      setFormData((prevFormData) => ({
        ...prevFormData,
        ...userConfig,
      }));
    } catch (err: any) {
      console.error('Error fetching existing bot:', err);
    }
  }, [user, botId, token]);

  useEffect(() => {
    const loadData = async () => {
      await fetchBotDetails();
      if (botId) {
        await fetchExistingBot();
      }
    };
    loadData();
  }, [name, version, botId, fetchBotDetails, fetchExistingBot]);

  // Handle error redirect
  React.useEffect(() => {
    if (error && !loading) {
      setTimeout(() => router.back(), 2000);
    }
  }, [error, loading, router]);

  // Handle access denied
  React.useEffect(() => {
    if (botDetails && !botDetails.hasAccess) {
      toast({
        title: 'Access Denied',
        description:
          "You don't have access to deploy this bot. Make sure you own this custom bot or have purchased it from the marketplace.",
        variant: 'destructive',
      });
      setTimeout(() => router.back(), 2000);
    }
  }, [botDetails, toast, router]);

  const handleFieldChange = (name: string, value: any) => {
    setFormData((prev) => ({ ...prev, [name]: value }));
  };

  // Debug: Log current form data
  React.useEffect(() => {
    console.log('Current form data:', formData);
  }, [formData]);

  const handleSubmit = async () => {
    if (!user || !botDetails) return;

    // For updates, show the confirmation dialog instead of submitting directly
    if (isUpdate) {
      setShowUpdateDialog(true);
      return;
    }

    // For new deployments, proceed directly
    await performDeployment();
  };

  const performDeployment = async () => {
    if (!user || !botDetails || !token) return;

    setSubmitting(true);
    try {

      // Create the bot configuration payload
      const botConfig = {
        // Include bot metadata with proper format: type/name
        type: `${botDetails.config.type}/${botDetails.config.name}`,
        version: botDetails.config.version,
        name: botDetails.config.name,
        description: botDetails.config.description,
        runtime: botDetails.config.runtime,
        author: botDetails.config.author,
        // Include user-provided configuration
        ...formData,
      };

      // Ensure scheduled bots have a schedule field (backend requirement)
      if (
        botDetails.config.type === 'scheduled' &&
        !(botConfig as any).schedule
      ) {
        throw new Error(
          'Schedule is required for scheduled bots. Please provide a valid cron expression.',
        );
      }

      const payload = isUpdate
        ? { config: botConfig }
        : { name: botDetails.config.name, config: botConfig };

      const response = await fetch(
        isUpdate ? `/api/bot/${botId}` : '/api/bot',
        {
          method: isUpdate ? 'PUT' : 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: `Bearer ${token}`,
          },
          body: JSON.stringify(payload),
        },
      );

      if (!response.ok) {
        const errorData = await response.json();
        throw new Error(
          errorData.error?.message || errorData.message || 'Deployment failed',
        );
      }

      await response.json();

      toast({
        title: isUpdate ? 'Bot Updated' : 'Bot Deployed',
        description: isUpdate
          ? 'Your bot configuration has been successfully updated.'
          : 'Your bot has been successfully deployed and is now running.',
      });

      // Redirect to dashboard or bot details
      router.push('/dashboard');
    } catch (err: any) {
      // Clear any previous errors
      setError(null);
      setValidationErrors({});

      // Show toast error
      toast({
        title: isUpdate ? 'Update Failed' : 'Deployment Failed',
        description: err.message,
        variant: 'destructive',
      });
    } finally {
      setSubmitting(false);
    }
  };

  const handleBack = () => {
    router.back();
  };

  if (!name || !version) {
    return (
      <DashboardLayout>
        <div className="container max-w-4xl py-6">
          <Alert variant="destructive">
            <AlertDescription>
              Invalid bot name or version parameters.
            </AlertDescription>
          </Alert>
        </div>
      </DashboardLayout>
    );
  }

  if (loading) {
    return (
      <DashboardLayout>
        <div className="container max-w-4xl py-6">
          <div className="flex items-center justify-center py-12">
            <Loader2 className="h-8 w-8 animate-spin" />
            <span className="ml-2">Loading bot details...</span>
          </div>
        </div>
      </DashboardLayout>
    );
  }

  if (error || !botDetails) {
    return (
      <DashboardLayout>
        <div className="container max-w-7xl py-6">
          <div className="flex items-center justify-center py-12">
            <Loader2 className="h-8 w-8 animate-spin" />
            <span className="ml-2">Loading bot details...</span>
          </div>
        </div>
      </DashboardLayout>
    );
  }

  if (!botDetails.hasAccess) {
    return (
      <DashboardLayout>
        <div className="container max-w-7xl py-6">
          <div className="flex items-center justify-center py-12">
            <div className="text-center">
              <h3 className="text-lg font-medium">Checking access...</h3>
              <p className="text-muted-foreground mt-1">
                Redirecting you back...
              </p>
            </div>
          </div>
        </div>
      </DashboardLayout>
    );
  }

  return (
    <DashboardLayout>
      <div className="container max-w-7xl py-6">
        {/* Header */}
        <div className="flex items-center gap-4 mb-6">
          <Button variant="outline" onClick={handleBack} className="gap-2">
            <ArrowLeft className="h-4 w-4" />
            Back
          </Button>
          <div className="flex items-center gap-3">
            <div className="flex items-center justify-center h-10 w-10 rounded-lg bg-primary/10">
              <Bot className="h-5 w-5 text-primary" />
            </div>
            <div>
              <h1 className="text-2xl font-bold">
                {isUpdate ? 'Update' : 'Deploy'} {botDetails.config.name}
              </h1>
              <p className="text-muted-foreground">
                Version {version} • {botDetails.config.type} bot
              </p>
            </div>
          </div>
        </div>

        {/* Two Column Layout on Desktop */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          {/* Left Column - Configuration Form */}
          <div className="space-y-6">
            <Card>
              <CardHeader>
                <CardTitle>Bot Configuration</CardTitle>
                <p className="text-sm text-muted-foreground">
                  {isUpdate
                    ? 'Update the configuration for your running bot instance.'
                    : 'Configure your bot parameters before deployment.'}
                </p>
              </CardHeader>
              <CardContent className="space-y-6">
                {botDetails.config.schema?.bot ? (
                  <FormGenerator
                    schema={botDetails.config.schema.bot as any}
                    formData={formData}
                    handleFieldChange={handleFieldChange}
                    errors={validationErrors}
                    botType={botDetails.config.type}
                  />
                ) : (
                  <Alert>
                    <AlertDescription>
                      This bot doesn&apos;t require any configuration
                      parameters.
                    </AlertDescription>
                  </Alert>
                )}

                {/* Action Buttons */}
                <div className="flex justify-end gap-3 pt-6 border-t">
                  <Button variant="outline" onClick={handleBack}>
                    Cancel
                  </Button>
                  <Button
                    onClick={handleSubmit}
                    disabled={submitting}
                    className="gap-2"
                  >
                    {submitting ? (
                      <Loader2 className="h-4 w-4 animate-spin" />
                    ) : isUpdate ? (
                      <Save className="h-4 w-4" />
                    ) : (
                      <Play className="h-4 w-4" />
                    )}
                    {submitting
                      ? isUpdate
                        ? 'Updating...'
                        : 'Deploying...'
                      : isUpdate
                        ? 'Update Bot'
                        : 'Deploy Bot'}
                  </Button>
                </div>
              </CardContent>
            </Card>
          </div>

          {/* Right Column - Documentation and Bot Info */}
          <div className="space-y-6">
            {/* Bot Metadata */}
            <Card>
              <CardHeader>
                <CardTitle>Bot Information</CardTitle>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="grid grid-cols-2 gap-4 text-sm">
                  <div>
                    <span className="text-muted-foreground">Type:</span>
                    <div className="font-medium">{botDetails.config.type}</div>
                  </div>
                  <div>
                    <span className="text-muted-foreground">Runtime:</span>
                    <div className="font-medium">
                      {botDetails.config.runtime}
                    </div>
                  </div>
                  <div>
                    <span className="text-muted-foreground">Version:</span>
                    <div className="font-medium">
                      v{botDetails.config.version}
                    </div>
                  </div>
                  <div>
                    <span className="text-muted-foreground">Author:</span>
                    <div className="font-medium">
                      {botDetails.config.author}
                    </div>
                  </div>
                </div>
                {botDetails.config.description && (
                  <div>
                    <span className="text-muted-foreground text-sm">
                      Description:
                    </span>
                    <p className="text-sm mt-1">
                      {botDetails.config.description}
                    </p>
                  </div>
                )}
              </CardContent>
            </Card>

            {/* Documentation */}
            <ReadmeComponent readme={botDetails.config.readme} />
          </div>
        </div>
      </div>

      {/* Update Confirmation Dialog */}
      {isUpdate && existingBot && (
        <BotUpdateConfirmationDialog
          isOpen={showUpdateDialog}
          onOpenChange={setShowUpdateDialog}
          onConfirm={async () => {
            setShowUpdateDialog(false);
            await performDeployment();
          }}
          isUpdating={submitting}
          botName={botDetails?.config.name || 'Unknown Bot'}
          oldConfig={existingBot.config}
          newConfig={{
            // Include bot metadata with proper format: type/name
            type: `${botDetails?.config.type}/${botDetails?.config.name}`,
            version: botDetails?.config.version,
            name: botDetails?.config.name,
            description: botDetails?.config.description,
            runtime: botDetails?.config.runtime,
            author: botDetails?.config.author,
            // Include user-provided configuration
            ...formData,
          }}
        />
      )}
    </DashboardLayout>
  );
}

export default withAuth(BotDeployPage);
