/** Parse a raw SSE message block into event type and data. */
export function parseSSEMessage(msg: string): {
  eventType: string;
  data: string;
} {
  let eventType = "message";
  let data = "";
  for (const line of msg.split("\n")) {
    if (line.startsWith("event: ")) eventType = line.slice(7).trim();
    else if (line.startsWith("data: ")) data += line.slice(6);
    else if (line.startsWith("data:")) data += line.slice(5);
  }
  return { eventType, data };
}

/**
 * Read an SSE response body until it ends, calling `onEvent` for every
 * message that carries data. Rejects if the underlying read rejects (abort,
 * network loss).
 */
export async function readSSEEvents(
  body: ReadableStream<Uint8Array>,
  onEvent: (eventType: string, data: string) => void,
): Promise<void> {
  const reader = body.getReader();
  const decoder = new TextDecoder();
  let buffer = "";

  while (true) {
    const { done, value } = await reader.read();
    if (done) return;

    buffer += decoder.decode(value, { stream: true });
    const messages = buffer.split("\n\n");
    buffer = messages.pop() || "";

    for (const msg of messages) {
      if (!msg.trim()) continue;
      const { eventType, data } = parseSSEMessage(msg);
      if (data) onEvent(eventType, data);
    }
  }
}
