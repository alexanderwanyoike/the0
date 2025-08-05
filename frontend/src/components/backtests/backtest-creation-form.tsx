"use client";

import React, { useState, useEffect } from "react";
import { useRouter } from "next/navigation";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { BotSelectionForm } from "./bot-selection-form";
import FormGenerator from "@/components/bots/form-generator";
import { useBacktestCreation } from "@/hooks/backtests/use-backtest-creation";
import { BotSearchResult } from "@/types/backtest";
import { PlayCircle, Loader2 } from "lucide-react";
import { authFetch } from "@/lib/auth-fetch";
import { BotType } from "@/types/custom-bots";
import { canBotBeBacktested } from "@/lib/utils";
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert";
import { AlertTriangle, XCircle } from "lucide-react";

interface BacktestCreationFormProps {
  preSelectedBotName?: string;
  preSelectedBotVersion?: string;
}

interface FormData {
  name: string;
  [key: string]: any;
}

export const BacktestCreationForm: React.FC<BacktestCreationFormProps> = ({
  preSelectedBotName,
  preSelectedBotVersion,
}) => {
  const router = useRouter();
  const [selectedBot, setSelectedBot] = useState<
    BotSearchResult | null | undefined
  >(null);
  const [selectedVersion, setSelectedVersion] = useState<string>("");
  const [formData, setFormData] = useState<FormData>({ name: "" });
  const [errors, setErrors] = useState<Record<string, string>>({});
  const [botSchema, setBotSchema] = useState<any>(null);
  const [botType, setBotType] = useState<BotType | null>(null);
  const [schemaLoading, setSchemaLoading] = useState(false);
  const [schemaError, setSchemaError] = useState<string | null>(null);
  const [canBacktest, setCanBacktest] = useState<boolean>(true);
  const { createBacktest, loading, error } = useBacktestCreation();

  // Fetch bot schema when a bot and version are selected
  useEffect(() => {
    if (selectedBot && selectedVersion) {
      const fetchBotSchema = async () => {
        setSchemaLoading(true);
        setSchemaError(null);
        setBotSchema(null);
        setCanBacktest(true);

        try {
          // Fetch the bot's backtest schema from the API
          const response = await authFetch(
            `/api/custom-bots/${encodeURIComponent(selectedBot.name)}/${encodeURIComponent(selectedVersion)}`,
          );

          const result = await response.json();

          if (result.success) {
            const config = result.data.config;
            setBotType(config.type);

            // Check if bot can be backtested
            if (canBotBeBacktested(config)) {
              setBotSchema(config.schema.backtest);
              setCanBacktest(true);
            } else {
              setCanBacktest(false);
              setSchemaError("This bot does not support backtesting.");
            }
          } else {
            setSchemaError(
              "Failed to load bot configuration. Please try again later.",
            );
          }
        } catch (err) {
          console.error("Error fetching bot schema:", err);
          setSchemaError(
            "Failed to load bot configuration. Please try again later.",
          );
        } finally {
          setSchemaLoading(false);
        }
      };

      fetchBotSchema();
    } else {
      setBotSchema(null);
      setSchemaError(null);
      setCanBacktest(true);
    }
  }, [selectedBot, selectedVersion]);

  // Sync URL when bot and version are selected
  useEffect(() => {
    if (selectedBot && selectedVersion) {
      // Update URL to reflect the selected bot name and version
      const newUrl = `/backtests/create?name=${encodeURIComponent(selectedBot.name)}&version=${encodeURIComponent(selectedVersion)}`;
      router.replace(newUrl, { scroll: false });
    }
  }, [selectedBot, selectedVersion, router]);

  const handleFieldChange = (name: string, value: any) => {
    setFormData((prev) => ({ ...prev, [name]: value }));
    // Clear error when field is modified
    if (errors[name]) {
      setErrors((prev) => ({ ...prev, [name]: "" }));
    }
  };

  const isFormValid = (): boolean => {
    // Check basic requirements
    if (!formData.name?.trim() || !selectedBot || !selectedVersion) {
      return false;
    }

    // Check schema requirements if schema exists
    if (botSchema && botSchema.required) {
      for (const field of botSchema.required) {
        if (!formData[field]) {
          return false;
        }
      }
    }

    // Additional validation for date fields
    if (formData.startDate && formData.endDate) {
      const startDate = new Date(formData.startDate);
      const endDate = new Date(formData.endDate);

      if (startDate >= endDate || endDate > new Date()) {
        return false;
      }
    }

    return true;
  };

  const validateForm = (): boolean => {
    const newErrors: Record<string, string> = {};

    // Validate name
    if (!formData.name?.trim()) {
      newErrors.name = "Backtest name is required";
    }

    // Validate bot selection
    if (!selectedBot) {
      newErrors.bot = "Please select a bot to backtest";
    } else if (!selectedVersion) {
      newErrors.bot = "Please select a version for the bot";
    }

    // Validate schema fields if bot and version are selected
    if (selectedBot && selectedVersion && botSchema) {
      botSchema.required?.forEach((field: string) => {
        if (!formData[field]) {
          newErrors[field] = `${field} is required`;
        }
      });
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!validateForm()) {
      return;
    }

    if (!selectedBot) {
      return;
    }

    // Prepare form data for submission
    const { name, ...configData } = formData;

    const backtestData = {
      name,
      config: {
        type: `${botType}/${selectedBot.name}`,
        version: selectedVersion,
        name,
        ...configData,
      },
    };

    await createBacktest(backtestData);
  };

  // Debug logging for BotSelectionForm props
  console.log("BotSelectionForm props:", {
    selectedBot,
    preSelectedBotName,
    preSelectedBotVersion,
    selectedVersion,
  });

  return (
    <form onSubmit={handleSubmit} className="space-y-6">
      {/* Backtest Name */}
      <Card>
        <CardHeader>
          <CardTitle>Backtest Details</CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div>
            <Label htmlFor="name">
              Backtest Name <span className="text-red-500">*</span>
            </Label>
            <Input
              id="name"
              type="text"
              value={formData.name}
              onChange={(e) => handleFieldChange("name", e.target.value)}
              placeholder="Enter a name for your backtest"
              className={errors.name ? "border-red-500" : ""}
            />
            {errors.name && (
              <p className="text-xs text-red-500 mt-1">{errors.name}</p>
            )}
          </div>
        </CardContent>
      </Card>

      {/* Bot Selection */}
      <Card>
        <CardHeader>
          <CardTitle>Bot Selection</CardTitle>
        </CardHeader>
        <CardContent>
          <BotSelectionForm
            selectedBot={selectedBot}
            onBotSelect={setSelectedBot}
            preSelectedBotName={preSelectedBotName}
            preSelectedBotVersion={preSelectedBotVersion}
            selectedVersion={selectedVersion}
            onVersionSelect={setSelectedVersion}
          />
          {errors.bot && (
            <p className="text-xs text-red-500 mt-2">{errors.bot}</p>
          )}
        </CardContent>
      </Card>

      {/* Dynamic Form Fields */}
      {selectedBot && selectedVersion && (
        <Card>
          <CardHeader>
            <CardTitle>Backtest Configuration</CardTitle>
          </CardHeader>
          <CardContent>
            {schemaLoading && (
              <div className="flex items-center justify-center py-8">
                <Loader2 className="w-6 h-6 animate-spin mr-2" />
                <span className="text-muted-foreground">
                  Loading bot configuration...
                </span>
              </div>
            )}

            {schemaError && (
              <Alert
                variant={!canBacktest ? "destructive" : "default"}
                className="mb-4"
              >
                {!canBacktest ? (
                  <XCircle className="h-4 w-4" />
                ) : (
                  <AlertTriangle className="h-4 w-4" />
                )}
                <AlertTitle>
                  {!canBacktest
                    ? "Backtesting Not Supported"
                    : "Configuration Schema Unavailable"}
                </AlertTitle>
                <AlertDescription>{schemaError}</AlertDescription>
              </Alert>
            )}

            {botSchema && (
              <FormGenerator
                schema={botSchema}
                formData={formData}
                handleFieldChange={handleFieldChange}
                errors={errors}
              />
            )}

            {selectedBot &&
              selectedVersion &&
              !schemaLoading &&
              !botSchema &&
              !schemaError && (
                <div className="text-center py-8 text-muted-foreground">
                  <p className="text-sm">
                    No additional configuration required for this bot.
                  </p>
                  <p className="text-xs mt-1">
                    You can proceed to create the backtest.
                  </p>
                </div>
              )}
          </CardContent>
        </Card>
      )}

      {/* Submit Button */}
      <Card>
        <CardContent className="pt-6">
          <div className="flex flex-col sm:flex-row gap-4 justify-between">
            <div className="flex-1">
              {error && <p className="text-sm text-red-500 mb-4">{error}</p>}
              <p className="text-sm text-muted-foreground">
                Once created, the backtest will be queued for execution and
                you&apos;ll be redirected to view the results.
              </p>
            </div>
            <Button
              type="submit"
              disabled={loading || !isFormValid() || !canBacktest}
              className="gap-2 min-w-[140px]"
            >
              {loading ? (
                <>
                  <Loader2 className="w-4 h-4 animate-spin" />
                  Creating...
                </>
              ) : (
                <>
                  <PlayCircle className="w-4 h-4" />
                  Run Backtest
                </>
              )}
            </Button>
          </div>
        </CardContent>
      </Card>
    </form>
  );
};
