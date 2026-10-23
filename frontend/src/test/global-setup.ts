// Daily log files are named by UTC day. Running every test 14 hours ahead of
// UTC makes code that reads the local date where it means the UTC one fail
// on any machine, not only on one whose zone is far from UTC. Setting TZ in
// a test file is too late: it has to happen before the test process reads it.
export default function globalSetup(): void {
  process.env.TZ = "Pacific/Kiritimati";
}
