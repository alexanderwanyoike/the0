import { parseSSEMessage, readSSEEvents } from "@/lib/sse/sse-stream-reader";

function streamOf(chunks: string[]): ReadableStream<Uint8Array> {
  const encoder = new TextEncoder();
  return new ReadableStream<Uint8Array>({
    start(controller) {
      chunks.forEach((c) => controller.enqueue(encoder.encode(c)));
      controller.close();
    },
  });
}

describe("parseSSEMessage", () => {
  it("reads the event type and concatenates data lines", () => {
    expect(parseSSEMessage('event: update\ndata: {"a":\ndata:1}')).toEqual({
      eventType: "update",
      data: '{"a":1}',
    });
  });

  it("defaults the event type to message", () => {
    expect(parseSSEMessage("data: hi")).toEqual({
      eventType: "message",
      data: "hi",
    });
  });
});

describe("readSSEEvents", () => {
  it("emits complete messages across chunk boundaries until the stream ends", async () => {
    const events: [string, string][] = [];
    await readSSEEvents(
      streamOf([
        "event: update\nda",
        "ta: one\n\nevent: history\ndata: two\n\n",
        "\n\nevent: update\ndata: three\n\n",
      ]),
      (type, data) => events.push([type, data]),
    );

    expect(events).toEqual([
      ["update", "one"],
      ["history", "two"],
      ["update", "three"],
    ]);
  });

  it("skips messages without data and an unterminated trailing message", async () => {
    const events: string[] = [];
    await readSSEEvents(
      streamOf([
        ": keepalive\n\nevent: update\n\nevent: update\ndata: partial",
      ]),
      (_type, data) => events.push(data),
    );

    expect(events).toEqual([]);
  });

  it("rejects when the body read fails", async () => {
    const body = new ReadableStream<Uint8Array>({
      start(controller) {
        controller.error(new TypeError("Failed to fetch"));
      },
    });

    await expect(readSSEEvents(body, () => {})).rejects.toThrow(
      "Failed to fetch",
    );
  });
});
