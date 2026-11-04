import { createServer } from "http";
import pinoHttp from "pino-http";
import { Writable } from "stream";
import request from "supertest";
import { requestLogging } from "../logger.module";

async function logOf(headers: Record<string, string>): Promise<string> {
  let logged = "";
  const sink = new Writable({
    write(chunk, _encoding, done) {
      logged += chunk.toString();
      done();
    },
  });
  const log = pinoHttp({ ...requestLogging, transport: undefined }, sink);
  const server = createServer((req, res) => {
    log(req, res);
    res.end("ok");
  });

  await request(server).get("/bot").set(headers);
  return logged;
}

describe("request logging", () => {
  it.each([
    ["x-api-key", "the0_secretkey"],
    ["authorization", "ApiKey the0_secretkey"],
    ["cookie", "session=secretkey"],
  ])("keeps the %s header out of the log", async (header, value) => {
    const logged = await logOf({ [header]: value });

    expect(logged).toContain("GET /bot 200");
    expect(logged).not.toContain("secretkey");
  });
});
