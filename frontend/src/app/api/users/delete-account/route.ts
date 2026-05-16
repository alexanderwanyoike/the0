import { NextRequest } from "next/server";
import { proxyBotApi } from "../../proxy";

export async function DELETE(req: NextRequest) {
  return proxyBotApi(req, "/users/delete-account", "DELETE", await req.json());
}
