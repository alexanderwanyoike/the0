'use client';

import React, { useState } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Alert, AlertDescription } from '@/components/ui/alert';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/components/ui/table';
import { TableData } from '@/types/backtest';
import {
  Table as TableIconLucide,
  Search,
  ArrowUpDown,
  ArrowUp,
  ArrowDown,
  AlertTriangle,
  FileText,
  ChevronLeft,
  ChevronRight,
} from 'lucide-react';

interface TablesDisplayProps {
  tables: TableData[];
}

interface SortConfig {
  key: string;
  direction: 'asc' | 'desc';
}

const TABLES_PER_PAGE = 8;

// Utility function to validate table data structure
const validateTableData = (data: any[]): boolean => {
  if (!Array.isArray(data) || data.length === 0) return false;

  const firstRowKeys = Object.keys(data[0] || {});
  if (firstRowKeys.length === 0) return false;

  // Check if all rows have consistent structure
  return data.every(
    (row) =>
      typeof row === 'object' &&
      row !== null &&
      !Array.isArray(row) &&
      Object.keys(row).length > 0,
  );
};

// Component to display non-tabular data
const NonTabularDataDisplay = ({
  title,
  data,
}: {
  title: string;
  data: any[];
}) => {
  const renderDataItem = (item: any, index: number) => {
    if (typeof item === 'object' && item !== null && !Array.isArray(item)) {
      return (
        <div key={index} className="p-3 border rounded-lg bg-muted/30">
          <div className="text-sm font-medium mb-2">Item {index + 1}</div>
          <div className="space-y-1 text-sm">
            {Object.entries(item).map(([key, value]) => (
              <div key={key}>
                <span className="font-medium text-muted-foreground">
                  {key.replace(/([A-Z])/g, ' $1').trim()}:
                </span>{' '}
                <span>{String(value)}</span>
              </div>
            ))}
          </div>
        </div>
      );
    }

    return (
      <div key={index} className="p-2 border rounded bg-muted/20 text-sm">
        {String(item)}
      </div>
    );
  };

  return (
    <div className="space-y-4">
      <Alert>
        <FileText className="h-4 w-4" />
        <AlertDescription>
          This data cannot be displayed as a table because it doesn&apos;t have
          a consistent structure. Showing raw data instead.
        </AlertDescription>
      </Alert>
      <div className="space-y-2 max-h-96 overflow-auto">
        {data.slice(0, 20).map((item, index) => renderDataItem(item, index))}
        {data.length > 20 && (
          <div className="text-center text-sm text-muted-foreground p-2">
            ... and {data.length - 20} more items
          </div>
        )}
      </div>
    </div>
  );
};

export function TablesDisplay({ tables }: TablesDisplayProps) {
  const [selectedTable, setSelectedTable] = useState(0);
  const [currentPage, setCurrentPage] = useState(0);
  const [searchTerm, setSearchTerm] = useState('');
  const [sortConfig, setSortConfig] = useState<SortConfig | null>(null);

  // Auto-navigate to page containing selected table - moved before early returns
  React.useEffect(() => {
    if (!tables || tables.length === 0) return;

    const totalPages = Math.ceil(tables.length / TABLES_PER_PAGE);
    const pageContainingSelection = Math.floor(selectedTable / TABLES_PER_PAGE);

    if (
      pageContainingSelection !== currentPage &&
      pageContainingSelection < totalPages
    ) {
      setCurrentPage(pageContainingSelection);
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [selectedTable, tables]);

  if (!tables || tables.length === 0) {
    return (
      <Card>
        <CardContent className="py-8">
          <div className="text-center">
            <TableIconLucide className="h-12 w-12 mx-auto mb-4 text-muted-foreground opacity-50" />
            <p className="text-center text-muted-foreground">
              No data tables available for this backtest.
            </p>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Process tables and separate valid from invalid ones
  const processedTables = tables.map((table, index) => ({
    index,
    table,
    isValid: validateTableData(table.data),
  }));

  const validTables = processedTables.filter((item) => item.isValid);
  const invalidTables = processedTables.filter((item) => !item.isValid);

  // Pagination for table selection
  const totalPages = Math.ceil(tables.length / TABLES_PER_PAGE);
  const startIndex = currentPage * TABLES_PER_PAGE;
  const currentPageTables = tables.slice(
    startIndex,
    startIndex + TABLES_PER_PAGE,
  );

  const selectedTableData = tables[selectedTable];
  const isValidTable = validateTableData(selectedTableData.data);

  const handleSort = (column: string) => {
    const newDirection =
      sortConfig?.key === column && sortConfig.direction === 'asc'
        ? 'desc'
        : 'asc';
    setSortConfig({ key: column, direction: newDirection });
  };

  const filterAndSortData = (data: Array<Record<string, any>>) => {
    let filteredData = [...data];

    // Apply search filter
    if (searchTerm) {
      filteredData = filteredData.filter((row) =>
        Object.values(row).some((value) =>
          String(value).toLowerCase().includes(searchTerm.toLowerCase()),
        ),
      );
    }

    // Apply sorting
    if (sortConfig) {
      filteredData.sort((a, b) => {
        const aValue = a[sortConfig.key];
        const bValue = b[sortConfig.key];

        // Handle different data types
        if (typeof aValue === 'number' && typeof bValue === 'number') {
          return sortConfig.direction === 'asc'
            ? aValue - bValue
            : bValue - aValue;
        }

        // Handle dates
        if (typeof aValue === 'string' && typeof bValue === 'string') {
          const aDate = new Date(aValue);
          const bDate = new Date(bValue);
          if (!isNaN(aDate.getTime()) && !isNaN(bDate.getTime())) {
            return sortConfig.direction === 'asc'
              ? aDate.getTime() - bDate.getTime()
              : bDate.getTime() - aDate.getTime();
          }
        }

        // Handle strings
        const aStr = String(aValue).toLowerCase();
        const bStr = String(bValue).toLowerCase();
        if (sortConfig.direction === 'asc') {
          return aStr.localeCompare(bStr);
        } else {
          return bStr.localeCompare(aStr);
        }
      });
    }

    return filteredData;
  };

  const formatCellValue = (value: any, key: string): string => {
    if (value === null || value === undefined) return '-';

    // Format numbers
    if (typeof value === 'number') {
      // Check if it's a percentage field
      if (
        key.toLowerCase().includes('rate') ||
        key.toLowerCase().includes('return') ||
        key.toLowerCase().includes('pct')
      ) {
        return `${(value * 100).toFixed(2)}%`;
      }
      // Check if it's a currency field
      if (
        key.toLowerCase().includes('price') ||
        key.toLowerCase().includes('value') ||
        key.toLowerCase().includes('pnl')
      ) {
        return `$${value.toFixed(2)}`;
      }
      // Regular number formatting
      return value % 1 === 0 ? value.toString() : value.toFixed(4);
    }

    // Format dates
    if (typeof value === 'string') {
      const date = new Date(value);
      if (!isNaN(date.getTime()) && value.includes('T')) {
        return (
          date.toLocaleDateString() +
          ' ' +
          date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })
        );
      }
    }

    return String(value);
  };

  const getSortIcon = (column: string) => {
    if (sortConfig?.key === column) {
      return sortConfig.direction === 'asc' ? (
        <ArrowUp className="h-3 w-3" />
      ) : (
        <ArrowDown className="h-3 w-3" />
      );
    }
    return <ArrowUpDown className="h-3 w-3" />;
  };

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <TableIconLucide className="h-5 w-5" />
          Data Tables
          {invalidTables.length > 0 && (
            <span className="text-sm text-muted-foreground">
              ({validTables.length} tabular, {invalidTables.length} non-tabular)
            </span>
          )}
        </CardTitle>
      </CardHeader>
      <CardContent>
        {invalidTables.length > 0 && (
          <Alert className="mb-4">
            <AlertTriangle className="h-4 w-4" />
            <AlertDescription>
              {invalidTables.length} table{invalidTables.length > 1 ? 's' : ''}{' '}
              cannot be displayed in table format due to inconsistent data
              structure.
            </AlertDescription>
          </Alert>
        )}

        <div className="grid grid-cols-1 lg:grid-cols-4 gap-8">
          {/* Table Selection Panel */}
          <div className="lg:col-span-1">
            <div className="flex items-center justify-between mb-6">
              <h3 className="text-sm font-medium text-foreground">Tables</h3>
              {totalPages > 1 && (
                <div className="flex items-center gap-2">
                  <Button
                    variant="ghost"
                    size="sm"
                    onClick={() => setCurrentPage(Math.max(0, currentPage - 1))}
                    disabled={currentPage === 0}
                    className="h-8 w-8 p-0"
                  >
                    <ChevronLeft className="h-4 w-4" />
                  </Button>
                  <span className="text-xs text-muted-foreground min-w-[40px] text-center">
                    {currentPage + 1} / {totalPages}
                  </span>
                  <Button
                    variant="ghost"
                    size="sm"
                    onClick={() =>
                      setCurrentPage(Math.min(totalPages - 1, currentPage + 1))
                    }
                    disabled={currentPage === totalPages - 1}
                    className="h-8 w-8 p-0"
                  >
                    <ChevronRight className="h-4 w-4" />
                  </Button>
                </div>
              )}
            </div>

            <div className="space-y-1">
              {currentPageTables.map((table, index) => {
                const globalIndex = startIndex + index;
                const isValid = validateTableData(table.data);
                return (
                  <button
                    key={globalIndex}
                    onClick={() => setSelectedTable(globalIndex)}
                    className={`w-full text-left px-3 py-2 rounded-lg transition-all duration-200 ${
                      selectedTable === globalIndex
                        ? 'bg-primary/10 text-primary border-l-3 border-primary pl-3'
                        : 'text-foreground hover:bg-accent'
                    }`}
                  >
                    <div className="flex items-center gap-3">
                      <TableIconLucide
                        className={`h-4 w-4 flex-shrink-0 ${
                          selectedTable === globalIndex
                            ? 'text-primary'
                            : 'text-muted-foreground'
                        }`}
                      />
                      <span className="text-sm truncate">
                        {table.title}
                        {!isValid && (
                          <span className="ml-1 text-xs opacity-60">*</span>
                        )}
                      </span>
                    </div>
                  </button>
                );
              })}
            </div>
          </div>

          {/* Table Display Panel */}
          <div className="lg:col-span-3">
            <div className="mb-4 flex items-center justify-between">
              <h3 className="text-lg font-normal text-foreground">
                {selectedTableData.title}
                {!isValidTable && (
                  <span className="text-sm text-muted-foreground ml-2">
                    (Non-tabular data)
                  </span>
                )}
              </h3>
              {isValidTable && (
                <div className="relative">
                  <Search className="absolute left-2 top-1/2 transform -translate-y-1/2 h-3 w-3 text-muted-foreground" />
                  <Input
                    placeholder="Search..."
                    value={searchTerm}
                    onChange={(e) => setSearchTerm(e.target.value)}
                    className="pl-8 h-8 w-48"
                  />
                </div>
              )}
            </div>

            {!isValidTable ? (
              <NonTabularDataDisplay
                title={selectedTableData.title}
                data={selectedTableData.data}
              />
            ) : (
              <>
                {(() => {
                  const filteredData = filterAndSortData(
                    selectedTableData.data,
                  );
                  const columns =
                    selectedTableData.data.length > 0
                      ? Object.keys(selectedTableData.data[0])
                      : [];

                  return (
                    <>
                      <div className="rounded-md border overflow-auto max-h-96">
                        <Table>
                          <TableHeader>
                            <TableRow>
                              {columns.map((column) => (
                                <TableHead
                                  key={column}
                                  className="whitespace-nowrap"
                                >
                                  <Button
                                    variant="ghost"
                                    size="sm"
                                    onClick={() => handleSort(column)}
                                    className="h-auto p-0 font-medium hover:bg-transparent"
                                  >
                                    <span className="capitalize">
                                      {column.replace(/([A-Z])/g, ' $1').trim()}
                                    </span>
                                    {getSortIcon(column)}
                                  </Button>
                                </TableHead>
                              ))}
                            </TableRow>
                          </TableHeader>
                          <TableBody>
                            {filteredData.length > 0 ? (
                              filteredData.map((row, rowIndex) => (
                                <TableRow key={rowIndex}>
                                  {columns.map((column) => (
                                    <TableCell
                                      key={column}
                                      className="whitespace-nowrap"
                                    >
                                      {formatCellValue(row[column], column)}
                                    </TableCell>
                                  ))}
                                </TableRow>
                              ))
                            ) : (
                              <TableRow>
                                <TableCell
                                  colSpan={columns.length}
                                  className="text-center py-8 text-muted-foreground"
                                >
                                  {searchTerm
                                    ? 'No matching results found'
                                    : 'No data available'}
                                </TableCell>
                              </TableRow>
                            )}
                          </TableBody>
                        </Table>
                      </div>
                      {filteredData.length > 0 && (
                        <p className="text-xs text-muted-foreground mt-2">
                          Showing {filteredData.length} of{' '}
                          {selectedTableData.data.length} rows
                        </p>
                      )}
                    </>
                  );
                })()}
              </>
            )}
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
