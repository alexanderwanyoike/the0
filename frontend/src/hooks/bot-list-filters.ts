export interface FilterOption {
  value: string;
  label: string;
}

export interface FilterFacet {
  label: string;
  value: string;
  options: readonly FilterOption[];
  onChange: (value: string) => void;
}

export interface BotListFilters<T> {
  search: string;
  setSearch: (value: string) => void;
  hasActiveFilters: boolean;
  activeCount: number;
  facets: FilterFacet[];
  filterBots: (bots: T[]) => T[];
}

export const BOT_TYPE_OPTIONS: readonly FilterOption[] = [
  { value: "all", label: "All" },
  { value: "scheduled", label: "Scheduled" },
  { value: "realtime", label: "Real-time" },
];
