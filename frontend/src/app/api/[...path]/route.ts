import { forwardToApi } from "@/app/api/proxy";

export const dynamic = "force-dynamic";

export {
  forwardToApi as GET,
  forwardToApi as POST,
  forwardToApi as PUT,
  forwardToApi as PATCH,
  forwardToApi as DELETE,
};
