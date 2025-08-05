import React, { JSX } from "react";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Switch } from "@/components/ui/switch";
import { Slider } from "@/components/ui/slider";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { Textarea } from "@/components/ui/textarea";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";

interface BotSchema {
  type: "object";
  properties: Record<string, SchemaProperty>;
  required: string[];
}

interface SchemaProperty {
  type: string;
  description?: string;
  default?: any;
  minimum?: number;
  maximum?: number;
  enum?: string[];
  items?: SchemaProperty;
  format?: string;
  properties?: Record<string, SchemaProperty>;
  required?: string[];
}

interface FormGeneratorProps {
  schema: BotSchema;
  formData: Record<string, any>;
  handleFieldChange: (name: string, value: any) => void;
  fieldFilter?: (key: string) => boolean;
  errors?: Record<string, string>;
  botType?: string; // Add bot type to determine if we need schedule field
}

const FormGenerator: React.FC<FormGeneratorProps> = ({
  schema,
  formData,
  handleFieldChange,
  fieldFilter = () => true,
  errors = {},
  botType,
}) => {
  const filteredProperties = Object.entries(schema.properties).filter(([key]) =>
    fieldFilter(key),
  );

  // Add schedule field for scheduled bots if not present in schema
  const needsScheduleField =
    botType === "scheduled" && !schema.properties.schedule;

  return (
    <div className="space-y-6">
      {/* Render schedule field first for scheduled bots */}
      {needsScheduleField && (
        <div>
          {getFormElementForProperty(
            "schedule",
            {
              type: "string",
              format: "cron",
              description:
                'Cron expression for scheduling bot execution (e.g., "*/5 * * * *" for every 5 minutes)',
            },
            formData.schedule,
            true, // schedule is required for scheduled bots
            handleFieldChange,
            errors.schedule,
            0,
          )}
        </div>
      )}

      {/* Render other schema properties */}
      {filteredProperties.map(([key, prop]) => (
        <div key={key}>
          {getFormElementForProperty(
            key,
            prop,
            formData[key],
            schema.required.includes(key),
            handleFieldChange,
            errors[key],
            0,
          )}
        </div>
      ))}
    </div>
  );
};

function getFormElementForProperty(
  key: string,
  property: SchemaProperty,
  value: any,
  required: boolean,
  onChange: (name: string, value: any) => void,
  error?: string,
  level: number = 0,
): JSX.Element {
  const id = `field-${key}`;
  const label = formatLabel(key);
  const description = property.description;

  const commonProps = {
    id,
    "aria-describedby": description ? `${id}-description` : undefined,
    "aria-invalid": error ? true : undefined,
  };

  let inputElement: JSX.Element;

  switch (property.type) {
    case "boolean":
      inputElement = (
        <div className="flex items-center space-x-2">
          <Switch
            {...commonProps}
            checked={value ?? property.default ?? false}
            onCheckedChange={(checked) => onChange(key, checked)}
          />
          <Label htmlFor={id} className="text-sm font-normal">
            {label} {required && <span className="text-red-500">*</span>}
          </Label>
        </div>
      );
      break;

    case "number":
    case "integer":
      if (property.minimum !== undefined && property.maximum !== undefined) {
        // Use slider for numbers with min/max range
        inputElement = (
          <div className="space-y-2">
            <Label htmlFor={id}>
              {label} {required && <span className="text-red-500">*</span>}
            </Label>
            <div className="px-3">
              <Slider
                {...commonProps}
                value={[value ?? property.default ?? property.minimum]}
                onValueChange={(values) => onChange(key, values[0])}
                min={property.minimum}
                max={property.maximum}
                step={property.type === "integer" ? 1 : 0.1}
                className="w-full"
              />
              <div className="flex justify-between text-xs text-muted-foreground mt-1">
                <span>{property.minimum}</span>
                <span className="font-medium">
                  {value ?? property.default ?? property.minimum}
                </span>
                <span>{property.maximum}</span>
              </div>
            </div>
          </div>
        );
      } else {
        // Regular number input
        inputElement = (
          <div className="space-y-2">
            <Label htmlFor={id}>
              {label} {required && <span className="text-red-500">*</span>}
            </Label>
            <Input
              {...commonProps}
              type="number"
              value={value ?? property.default ?? ""}
              onChange={(e) => {
                const val = e.target.value;
                onChange(
                  key,
                  val === ""
                    ? undefined
                    : property.type === "integer"
                      ? parseInt(val)
                      : parseFloat(val),
                );
              }}
              min={property.minimum}
              max={property.maximum}
              step={property.type === "integer" ? 1 : "any"}
            />
          </div>
        );
      }
      break;

    case "string":
      if (
        key.toLowerCase() === "schedule" ||
        property.format === "cron" ||
        (property.description &&
          property.description.toLowerCase().includes("cron"))
      ) {
        // Schedule/Cron input with helper link
        inputElement = (
          <div className="space-y-2">
            <Label htmlFor={id}>
              {label} {required && <span className="text-red-500">*</span>}
            </Label>
            <Input
              {...commonProps}
              type="text"
              value={value ?? property.default ?? ""}
              onChange={(e) => onChange(key, e.target.value)}
              placeholder="*/5 * * * * (every 5 minutes)"
              className="font-mono"
            />
            <div className="flex items-center justify-between text-xs text-muted-foreground">
              <p>Enter a valid cron expression</p>
              <a
                href="https://crontab.guru"
                target="_blank"
                rel="noopener noreferrer"
                className="text-blue-600 hover:text-blue-800 underline"
              >
                Need help? Visit crontab.guru →
              </a>
            </div>
          </div>
        );
      } else if (property.enum) {
        // Enum/select dropdown
        inputElement = (
          <div className="space-y-2">
            <Label htmlFor={id}>
              {label} {required && <span className="text-red-500">*</span>}
            </Label>
            <Select
              value={value ?? property.default ?? ""}
              onValueChange={(val) => onChange(key, val)}
            >
              <SelectTrigger {...commonProps}>
                <SelectValue placeholder={`Select ${label.toLowerCase()}`} />
              </SelectTrigger>
              <SelectContent>
                {property.enum.map((option) => (
                  <SelectItem key={option} value={option}>
                    {formatLabel(option)}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>
        );
      } else if (
        property.format === "textarea" ||
        (description && description.length > 100)
      ) {
        // Textarea for long text
        inputElement = (
          <div className="space-y-2">
            <Label htmlFor={id}>
              {label} {required && <span className="text-red-500">*</span>}
            </Label>
            <Textarea
              {...commonProps}
              value={value ?? property.default ?? ""}
              onChange={(e) => onChange(key, e.target.value)}
              placeholder={`Enter ${label.toLowerCase()}`}
              rows={4}
            />
          </div>
        );
      } else {
        // Regular text input
        inputElement = (
          <div className="space-y-2">
            <Label htmlFor={id}>
              {label} {required && <span className="text-red-500">*</span>}
            </Label>
            <Input
              {...commonProps}
              type={property.format === "password" ? "password" : "text"}
              value={value ?? property.default ?? ""}
              onChange={(e) => onChange(key, e.target.value)}
              placeholder={`Enter ${label.toLowerCase()}`}
            />
          </div>
        );
      }
      break;

    case "array":
      // Simple array handling (comma-separated values)
      inputElement = (
        <div className="space-y-2">
          <Label htmlFor={id}>
            {label} {required && <span className="text-red-500">*</span>}
          </Label>
          <Input
            {...commonProps}
            type="text"
            value={
              Array.isArray(value)
                ? value.join(", ")
                : (value ?? property.default ?? "")
            }
            onChange={(e) => {
              const val = e.target.value;
              const array = val
                ? val.split(",").map((item) => item.trim())
                : [];
              onChange(key, array);
            }}
            placeholder={`Enter ${label.toLowerCase()} (comma-separated)`}
          />
          <p className="text-xs text-muted-foreground">
            Separate multiple values with commas
          </p>
        </div>
      );
      break;

    case "object":
      // Nested object handling
      if (property.properties) {
        const objectValue = value || {};
        inputElement = (
          <div className="space-y-4">
            <Label className="text-sm font-medium">
              {label} {required && <span className="text-red-500">*</span>}
            </Label>
            <div
              className={`border rounded-lg p-4 space-y-4 ${level > 0 ? "bg-muted/50" : "bg-muted/20"}`}
            >
              {Object.entries(property.properties).map(
                ([nestedKey, nestedProp]) => (
                  <div key={nestedKey}>
                    {getFormElementForProperty(
                      nestedKey,
                      nestedProp,
                      objectValue[nestedKey],
                      property.required?.includes(nestedKey) || false,
                      (nestedField, nestedValue) => {
                        const newObjectValue = {
                          ...objectValue,
                          [nestedField]: nestedValue,
                        };
                        onChange(key, newObjectValue);
                      },
                      undefined,
                      level + 1,
                    )}
                  </div>
                ),
              )}
            </div>
          </div>
        );
      } else {
        // Generic object input (JSON)
        inputElement = (
          <div className="space-y-2">
            <Label htmlFor={id}>
              {label} {required && <span className="text-red-500">*</span>}
            </Label>
            <Textarea
              {...commonProps}
              value={
                typeof value === "object"
                  ? JSON.stringify(value, null, 2)
                  : (value ?? property.default ?? "{}")
              }
              onChange={(e) => {
                try {
                  const parsed = JSON.parse(e.target.value);
                  onChange(key, parsed);
                } catch {
                  // Invalid JSON, keep as string for now
                  onChange(key, e.target.value);
                }
              }}
              placeholder={`Enter ${label.toLowerCase()} as JSON`}
              rows={4}
              className="font-mono text-sm"
            />
            <p className="text-xs text-muted-foreground">
              Enter a valid JSON object
            </p>
          </div>
        );
      }
      break;

    default:
      // Fallback to text input
      inputElement = (
        <div className="space-y-2">
          <Label htmlFor={id}>
            {label} {required && <span className="text-red-500">*</span>}
          </Label>
          <Input
            {...commonProps}
            type="text"
            value={value ?? property.default ?? ""}
            onChange={(e) => onChange(key, e.target.value)}
            placeholder={`Enter ${label.toLowerCase()}`}
          />
        </div>
      );
  }

  return (
    <div className="space-y-1">
      {inputElement}
      {description && (
        <p id={`${id}-description`} className="text-xs text-muted-foreground">
          {description}
        </p>
      )}
      {error && (
        <p className="text-xs text-red-500" role="alert">
          {error}
        </p>
      )}
    </div>
  );
}

function formatLabel(key: string): string {
  return key
    .replace(/([A-Z])/g, " $1")
    .replace(/^./, (str) => str.toUpperCase())
    .replace(/_/g, " ")
    .trim();
}

export default FormGenerator;
