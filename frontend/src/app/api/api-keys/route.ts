import { NextRequest } from "next/server";
import { withAdminAuth } from "@/lib/middleware/admin-auth";
import { proxyBotApiWithErrorEnvelope } from "@/app/api/proxy";

export async function GET(req: NextRequest) {
  return withAdminAuth(req, (req) =>
    proxyBotApiWithErrorEnvelope(
      req,
      "/api-keys",
      "GET",
      "Error fetching API keys",
    ),
  );
}

export async function POST(req: NextRequest) {
  return withAdminAuth(req, (req) =>
    proxyBotApiWithErrorEnvelope(
      req,
      "/api-keys",
      "POST",
      "Error creating API key",
      { forwardJsonBody: true },
    ),
  );
}
