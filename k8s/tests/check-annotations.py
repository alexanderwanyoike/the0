#!/usr/bin/env python3
"""Checks the Artifact Hub annotations in Chart.yaml.

helm lint treats annotations as opaque strings, so an Artifact Hub annotation
that is not valid YAML still lints and packages; Artifact Hub then refuses to
index that chart version. Prints "ok", or one line per problem.
"""

import re
import sys

import yaml

CHANGE_KINDS = {"added", "changed", "deprecated", "removed", "fixed", "security"}
YAML_ANNOTATIONS = ("artifacthub.io/changes", "artifacthub.io/images", "artifacthub.io/links")


def problems(chart_path):
    with open(chart_path) as f:
        annotations = yaml.safe_load(f).get("annotations", {})

    found = []
    for key in YAML_ANNOTATIONS:
        raw = annotations.get(key)
        if raw is None:
            continue
        try:
            yaml.safe_load(raw)
        except yaml.YAMLError as e:
            found.append(f"{key} is not valid YAML: {' '.join(str(e).split())}")

    raw_changes = annotations.get("artifacthub.io/changes", "")
    # Artifact Hub asks for quoted descriptions, since characters such as : - < >
    # in a plain scalar break or change how the YAML parses.
    for line in raw_changes.splitlines():
        match = re.match(r"\s*(?:-\s+)?description:\s*(.*)$", line)
        if match and not match.group(1).startswith(('"', "'")):
            found.append(f"unquoted changes description: {match.group(1)[:60]}")

    try:
        changes = yaml.safe_load(raw_changes) or []
    except yaml.YAMLError:
        changes = []
    for change in changes:
        if not isinstance(change, dict) or change.get("kind") not in CHANGE_KINDS:
            found.append(f"changes entry needs a kind from {sorted(CHANGE_KINDS)}: {change}")
        elif not change.get("description"):
            found.append(f"changes entry has no description: {change}")
    return found


if __name__ == "__main__":
    issues = problems(sys.argv[1])
    print("\n".join(issues) if issues else "ok")
