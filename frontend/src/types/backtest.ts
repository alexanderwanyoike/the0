import { BotType } from './custom-bots';

// Core type definitions for backtest functionality
export interface Backtest {
  id: string;
  name: string;
  config: BacktestConfig;
  analysis: BacktestAnalysis | null;
  status: 'pending' | 'running' | 'completed' | 'failed';
  createdAt: Date;
  updatedAt: Date;
  userId: string;
  customBotId: string;
}

export interface BacktestConfig {
  type: string; // Format: "botType/botName"
  version: string; // Bot version
  [key: string]: any; // Additional bot-specific parameters
}

export interface BacktestAnalysis {
  // Legacy flat structure (for backward compatibility)
  metrics?: Record<string, any>; // Changed from number | string to any for flexibility
  plots?: (PlotData | string)[]; // Support both object and stringified formats
  tables?: TableData[];
  // Error state fields
  status?: 'error' | 'success';
  message?: string; // Error message for failed analysis
  // New nested structure
  results?: {
    metrics?: Record<string, any>; // Changed from number | string to any for flexibility
    plots?: (PlotData | string)[]; // Support both object and stringified formats
    tables?: TableData[];
  };
}

export interface PlotData {
  title: string;
  data: any; // Raw Plotly data object (can be array of traces or single trace)
  layout?: any; // Optional Plotly layout configuration
  config?: any; // Optional Plotly config
}

export interface TableData {
  title: string;
  data: Array<Record<string, any>>;
}

// Add utility types for flexible metric handling
export type MetricValue =
  | string
  | number
  | boolean
  | MetricValue[]
  | { [key: string]: MetricValue };

export interface DynamicMetric {
  key: string;
  value: MetricValue;
  type: 'primitive' | 'array' | 'object';
}

export interface BotSearchResult {
  id: string;
  name: string;
  resultType: 'marketplace' | 'custom' | 'user';
  approved?: boolean;
}

// API Request/Response types
export interface CreateBacktestRequest {
  name: string;
  config: BacktestConfig;
}

export interface CreateBacktestResponse {
  id: string;
  name: string;
  config: BacktestConfig;
  analysis: string;
  status: 'pending' | 'running' | 'completed' | 'failed';
  createdAt: Date;
  updatedAt: Date;
  userId: string;
  customBotId: string;
}

// API compatible type for API operations
export interface BacktestDocument {
  id: string;
  name: string;
  config: BacktestConfig;
  analysis: BacktestAnalysis | null;
  status: 'pending' | 'running' | 'completed' | 'failed';
  createdAt: string; // ISO string instead of Timestamp
  updatedAt: string; // ISO string instead of Timestamp
  userId: string;
  customBotId: string;
}
