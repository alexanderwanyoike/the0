#!/usr/bin/env bash
set -euo pipefail

mkdir -p reports summaries

trivy fs \
  --scanners secret,misconfig \
  --format json \
  --output reports/trivy-repo.json \
  --severity UNKNOWN,LOW,MEDIUM,HIGH,CRITICAL \
  .

jq -r '
  def count_misconf($sev):
    [ .Results[]? | .Misconfigurations[]? | select(.Severity == $sev) | (.ID // .RuleID // .Title // "unknown") ] | unique | length;
  def count_secret($sev):
    [ .Results[]? | .Secrets[]? | select((.Severity // "UNKNOWN") == $sev) | (.RuleID // .Title // .Target // "unknown") ] | unique | length;
  {
    critical: (count_misconf("CRITICAL") + count_secret("CRITICAL")),
    high: (count_misconf("HIGH") + count_secret("HIGH")),
    medium: (count_misconf("MEDIUM") + count_secret("MEDIUM")),
    low: (count_misconf("LOW") + count_secret("LOW")),
    unknown: (count_misconf("UNKNOWN") + count_secret("UNKNOWN"))
  }
  | "### Trivy repository scan\n\n" +
    "| Severity | Unique findings |\n| --- | ---: |\n" +
    "| CRITICAL | \(.critical) |\n" +
    "| HIGH | \(.high) |\n" +
    "| MEDIUM | \(.medium) |\n" +
    "| LOW | \(.low) |\n" +
    "| UNKNOWN | \(.unknown) |\n"
' reports/trivy-repo.json > summaries/trivy-repo.md
