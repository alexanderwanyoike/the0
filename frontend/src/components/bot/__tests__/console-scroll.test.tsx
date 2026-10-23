import React from "react";
import { render, screen } from "@testing-library/react";
import { ConsoleInterface } from "../console-interface";
import type { LogEntry } from "@/types/logs";

const mockScrollToIndex = jest.fn();

jest.mock("react-virtuoso", () => {
  const ReactActual = jest.requireActual<typeof import("react")>("react");
  return {
    Virtuoso: ReactActual.forwardRef(function MockVirtuoso(
      {
        data,
        itemContent,
        initialTopMostItemIndex,
      }: {
        data: LogEntry[];
        itemContent: (index: number, item: LogEntry) => React.ReactNode;
        initialTopMostItemIndex?: number;
      },
      ref: React.Ref<{ scrollToIndex: typeof mockScrollToIndex }>,
    ) {
      ReactActual.useImperativeHandle(ref, () => ({
        scrollToIndex: mockScrollToIndex,
      }));
      return (
        <div
          data-testid="virtuoso"
          data-initial-index={String(initialTopMostItemIndex)}
        >
          {data.map((item, index) => (
            <div key={item.key}>{itemContent(index, item)}</div>
          ))}
        </div>
      );
    }),
  };
});

const line = (n: number): LogEntry => ({
  key: `1792713599999000000-${n}`,
  date: "20261022",
  timestamp: `2026-10-22T10:00:0${n}.000Z`,
  content: JSON.stringify({ message: `line ${n}` }),
});

const props = {
  botId: "bot-1",
  loading: false,
  onRefresh: jest.fn(),
  onDateChange: jest.fn(),
  onDateRangeChange: jest.fn(),
  onExport: jest.fn(),
  compact: true,
};

describe("console scroll position", () => {
  beforeEach(() => mockScrollToIndex.mockClear());

  describe("opening a view", () => {
    it("starts a chronological view at its newest line, the bottom", () => {
      render(
        <ConsoleInterface {...props} sort="asc" logs={[1, 2, 3].map(line)} />,
      );
      expect(screen.getByTestId("virtuoso")).toHaveAttribute(
        "data-initial-index",
        "2",
      );
    });

    it("starts a newest-first view at its newest line, the top", () => {
      render(
        <ConsoleInterface {...props} sort="desc" logs={[3, 2, 1].map(line)} />,
      );
      expect(screen.getByTestId("virtuoso")).toHaveAttribute(
        "data-initial-index",
        "0",
      );
    });
  });

  describe("switching to another window", () => {
    it("shows the new window from its newest line once its lines arrive", () => {
      const view = (viewKey: string, logs: LogEntry[]) => (
        <ConsoleInterface {...props} sort="asc" viewKey={viewKey} logs={logs} />
      );
      const liveLines = [1, 2, 3].map(line);
      const { rerender } = render(view("live", liveLines));

      // The old window's lines stay up until the new ones load
      rerender(view("1h", liveLines));
      expect(mockScrollToIndex).not.toHaveBeenCalled();

      rerender(view("1h", [2, 3, 4, 5, 6].map(line)));
      expect(mockScrollToIndex).toHaveBeenCalledWith({
        index: 4,
        align: "end",
      });
    });

    it("forgets the jump when the new window has no lines", () => {
      const view = (viewKey: string, logs: LogEntry[]) => (
        <ConsoleInterface {...props} sort="asc" viewKey={viewKey} logs={logs} />
      );
      const { rerender } = render(view("live", [1, 2, 3].map(line)));
      rerender(view("1h", []));
      rerender(view("1h", [4].map(line)));
      rerender(view("1h", [4, 5].map(line)));
      expect(mockScrollToIndex).not.toHaveBeenCalled();
    });

    it("forgets the jump when the new window fails to load", () => {
      const view = (viewKey: string, logs: LogEntry[], fetching: boolean) => (
        <ConsoleInterface
          {...props}
          sort="asc"
          viewKey={viewKey}
          logs={logs}
          fetching={fetching}
        />
      );
      const shown = [1, 2, 3].map(line);
      const { rerender } = render(view("live", shown, false));
      rerender(view("1h", shown, true));
      rerender(view("1h", shown, false));
      rerender(view("1h", [1, 2, 3, 4].map(line), false));
      expect(mockScrollToIndex).not.toHaveBeenCalled();
    });

    it("keeps the reader's place when earlier or live lines join", () => {
      const view = (logs: LogEntry[]) => (
        <ConsoleInterface {...props} sort="asc" viewKey="live" logs={logs} />
      );
      const { rerender } = render(view([3, 4].map(line)));
      rerender(view([1, 2, 3, 4].map(line)));
      rerender(view([1, 2, 3, 4, 5].map(line)));
      expect(mockScrollToIndex).not.toHaveBeenCalled();
    });
  });
});
