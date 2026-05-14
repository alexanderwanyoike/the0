const http = require("http");

const port = process.env.PORT || "3000";
const path = process.env.HEALTHCHECK_PATH || "/";

const request = http.get(
  {
    host: "127.0.0.1",
    port,
    path,
    timeout: 2000,
  },
  (response) => {
    response.resume();
    process.exit(response.statusCode === 200 ? 0 : 1);
  },
);

request.on("timeout", () => {
  request.destroy();
  process.exit(1);
});

request.on("error", () => {
  process.exit(1);
});
