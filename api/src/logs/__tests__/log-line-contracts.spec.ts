import { BOT_LOG_TOPICS } from "@/bot/bot.constants";
import { loadContract } from "@/test/contracts";
import { logsServiceServing } from "@/test/logs-service-serving";
import { LogEntry, liveLogEntries } from "../log-entry";

const contract = loadContract<{
  botId: string;
  subject: string;
  chunk: { at: string; raw: string; formatted: string };
  entries: LogEntry[];
}>("log-lines.json");

describe("log line contracts", () => {
  it("subscribes to a bot's logs on the subject the runtime publishes on", () => {
    expect(BOT_LOG_TOPICS.forBot(contract.botId)).toBe(contract.subject);
  });

  it("reads a runtime-formatted chunk from NATS as the contract's entries", () => {
    const receivedLater = new Date("2026-11-01T00:00:03.000Z");

    expect(liveLogEntries(contract.chunk.formatted, receivedLater, 7)).toEqual(
      contract.entries,
    );
  });

  it("reads a runtime-formatted daily file as the same entries", async () => {
    const { service } = logsServiceServing(contract.chunk.formatted);

    const page = await service.getLogs(
      contract.botId,
      { date: contract.entries[0].date, limit: 100, offset: 0, sort: "asc" },
      "user-1",
    );

    expect(page.data?.entries).toEqual(contract.entries);
  });
});
