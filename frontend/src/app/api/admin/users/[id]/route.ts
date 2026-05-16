import { NextRequest } from "next/server";
import { proxyBotApi } from "../../../proxy";

export async function PATCH(
  req: NextRequest,
  { params }: { params: Promise<{ id: string }> },
) {
  const { id } = await params;
  return proxyBotApi(req, `/admin/users/${id}`, "PATCH", await req.json());
}
