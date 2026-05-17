import { NextRequest } from "next/server";
import { isResponse, proxyBotApi, readJsonRequest } from "@/app/api/proxy";

export async function PUT(req: NextRequest) {
  const body = await readJsonRequest(req);
  if (isResponse(body)) return body;
  return proxyBotApi(req, "/users/change-password", "PUT", body);
}
