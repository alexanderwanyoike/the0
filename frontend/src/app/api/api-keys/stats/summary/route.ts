import { NextRequest } from "next/server";
import { withAdminAuth } from "@/lib/middleware/admin-auth";
import { proxyBotApiWithErrorEnvelope } from "@/app/api/proxy";

export async function GET(req: NextRequest) {
  return withAdminAuth(req, (req) =>
    proxyBotApiWithErrorEnvelope(
      req,
      "/api-keys/stats/summary",
      "GET",
      "Error fetching API key stats",
    ),
  );
}
