#!/usr/bin/env bash
# Usage: summary-code-block.sh <title> <command> [args...]
#
# Runs the command, appends its output to the job summary as a code block
# under a heading, and exits with the command's status. Backticks are escaped
# because the output can contain file names from the pull request, and a name
# with backticks could otherwise close the fence and inject Markdown.
set -u

title="$1"
shift

{
  echo "### ${title}"
  echo '```'
} >> "$GITHUB_STEP_SUMMARY"

"$@" 2>&1 | sed 's/`/\\`/g' >> "$GITHUB_STEP_SUMMARY"
status=${PIPESTATUS[0]}

echo '```' >> "$GITHUB_STEP_SUMMARY"
exit "$status"
