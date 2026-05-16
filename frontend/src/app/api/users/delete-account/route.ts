import { NextRequest } from "next/server";
import { isResponse, proxyBotApi, readJsonRequest } from "@/app/api/proxy";

export async function DELETE(req: NextRequest) {
  const body = await readJsonRequest(req);
  if (isResponse(body)) return body;
  return proxyBotApi(req, "/users/delete-account", "DELETE", body);
}
