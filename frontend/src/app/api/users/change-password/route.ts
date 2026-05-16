import { NextRequest } from "next/server";
import { proxyBotApi } from "@/app/api/proxy";

export async function PUT(req: NextRequest) {
  return proxyBotApi(req, "/users/change-password", "PUT", await req.json());
}
