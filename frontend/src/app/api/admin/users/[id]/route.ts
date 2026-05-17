import { NextRequest } from "next/server";
import { isResponse, proxyBotApi, readJsonRequest } from "@/app/api/proxy";

export async function PATCH(
  req: NextRequest,
  { params }: { params: Promise<{ id: string }> },
) {
  const { id } = await params;
  const body = await readJsonRequest(req);
  if (isResponse(body)) return body;
  return proxyBotApi(
    req,
    `/admin/users/${encodeURIComponent(id)}`,
    "PATCH",
    body,
  );
}
