import { NextRequest } from "next/server";
import { withAdminAuth } from "@/lib/middleware/admin-auth";
import { proxyBotApiWithErrorEnvelope } from "@/app/api/proxy";

type Params = { params: Promise<{ id: string }> };

export async function GET(req: NextRequest, { params }: Params) {
  return withAdminAuth(req, async (req) =>
    proxyBotApiWithErrorEnvelope(
      req,
      `/api-keys/${(await params).id}`,
      "GET",
      "Error fetching API key",
    ),
  );
}

export async function DELETE(req: NextRequest, { params }: Params) {
  return withAdminAuth(req, async (req) =>
    proxyBotApiWithErrorEnvelope(
      req,
      `/api-keys/${(await params).id}`,
      "DELETE",
      "Error deleting API key",
    ),
  );
}
