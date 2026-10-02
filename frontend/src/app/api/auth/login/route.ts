import { NextRequest } from "next/server";
import { proxyAuthPost } from "@/app/api/proxy";

export async function POST(req: NextRequest) {
  return proxyAuthPost(req, "login");
}
