import { NextRequest } from "next/server";
import { proxyBotApi } from "@/app/api/proxy";

export async function GET(req: NextRequest) {
  return proxyBotApi(req, "/admin/users", "GET");
}

export async function POST(req: NextRequest) {
  return proxyBotApi(req, "/admin/users", "POST", await req.json());
}
