"""Parser and mutator for .notes/issues-audit.md"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from pathlib import Path

# Matches: ### 12. [IN-PROGRESS: fix/audit-12-foo] Title here
# Groups: number, optional status, optional status detail, title
HEADING_RE = re.compile(
    r"^###\s+(\d+)\.\s+"
    r"(?:\[("
    r"RESOLVED|IN-PROGRESS|ATTEMPTED"
    r")(?::\s*([^\]]*))?\]\s*)?"
    r"(.+)$"
)

SECTION_RE = re.compile(r"^##\s+(.+)$")


@dataclass
class Issue:
    number: int
    title: str
    severity: str  # "CRITICAL/HIGH", "MEDIUM", "LOW"
    status: str | None = None  # None, "RESOLVED", "IN-PROGRESS", "ATTEMPTED"
    status_detail: str = ""  # branch name, failure reason, etc.
    body: list[str] = field(default_factory=list)
    line_number: int = 0  # 1-indexed line of the ### heading


@dataclass
class AuditFile:
    """Structured representation of issues-audit.md"""

    # Raw lines before the first ## section
    header: list[str] = field(default_factory=list)
    # Ordered list of (section_heading, separator_lines, issues)
    sections: list[tuple[str, list[str], list[Issue]]] = field(default_factory=list)
    # Lines after the last issue (trailing content)
    footer: list[str] = field(default_factory=list)

    @classmethod
    def parse(cls, path: Path) -> AuditFile:
        """Parse an issues-audit.md file into structured data."""
        text = path.read_text() if path.exists() else ""
        return cls.parse_text(text)

    @classmethod
    def parse_text(cls, text: str) -> AuditFile:
        af = cls()
        lines = text.splitlines(keepends=True)

        current_section: str | None = None
        current_separator: list[str] = []
        current_issues: list[Issue] = []
        current_issue: Issue | None = None

        for i, raw_line in enumerate(lines):
            line = raw_line.rstrip("\n")
            line_num = i + 1

            # Check for section heading (## CRITICAL / HIGH, ## MEDIUM, ## LOW)
            section_match = SECTION_RE.match(line)
            if section_match and not line.startswith("###"):
                # Flush current issue
                if current_issue:
                    current_issues.append(current_issue)
                    current_issue = None

                # Flush previous section
                if current_section is not None:
                    af.sections.append((current_section, current_separator, current_issues))

                section_text = section_match.group(1).strip()
                # Normalize section name
                if "CRITICAL" in section_text.upper() or "HIGH" in section_text.upper():
                    current_section = "CRITICAL/HIGH"
                elif "MEDIUM" in section_text.upper():
                    current_section = "MEDIUM"
                elif "LOW" in section_text.upper():
                    current_section = "LOW"
                else:
                    current_section = section_text

                current_separator = [raw_line]
                current_issues = []
                continue

            # Check for issue heading (### N. [STATUS] Title)
            heading_match = HEADING_RE.match(line)
            if heading_match:
                # Flush previous issue
                if current_issue:
                    current_issues.append(current_issue)

                num = int(heading_match.group(1))
                status = heading_match.group(2)  # None or "RESOLVED" etc.
                detail = heading_match.group(3) or ""
                title = heading_match.group(4).strip()

                current_issue = Issue(
                    number=num,
                    title=title,
                    severity=current_section or "UNKNOWN",
                    status=status,
                    status_detail=detail.strip(),
                    line_number=line_num,
                )
                continue

            # Accumulate lines
            if current_issue is not None:
                current_issue.body.append(raw_line)
            elif current_section is not None:
                current_separator.append(raw_line)
            else:
                af.header.append(raw_line)

        # Flush final issue and section
        if current_issue:
            current_issues.append(current_issue)
        if current_section is not None:
            af.sections.append((current_section, current_separator, current_issues))

        return af

    def pick_eligible_issue(self) -> Issue | None:
        """Return the first eligible issue (MEDIUM first, then LOW). Skips CRITICAL/HIGH."""
        for severity in ("MEDIUM", "LOW"):
            for section_name, _, issues in self.sections:
                if section_name != severity:
                    continue
                for issue in issues:
                    if issue.status is None:
                        return issue
        return None

    def mark_in_progress(self, issue: Issue, branch_name: str) -> None:
        issue.status = "IN-PROGRESS"
        issue.status_detail = branch_name

    def mark_attempted(self, issue: Issue, reason: str) -> None:
        issue.status = "ATTEMPTED"
        issue.status_detail = reason

    def mark_resolved(self, issue: Issue) -> None:
        issue.status = "RESOLVED"
        issue.status_detail = ""

    def write(self, path: Path) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(self.to_text())

    def to_text(self) -> str:
        parts: list[str] = []
        parts.extend(self.header)

        for section_name, separator, issues in self.sections:
            parts.extend(separator)
            for issue in issues:
                parts.append(self._format_heading(issue) + "\n")
                parts.extend(issue.body)

        parts.extend(self.footer)
        return "".join(parts)

    @staticmethod
    def _format_heading(issue: Issue) -> str:
        if issue.status:
            if issue.status_detail:
                tag = f"[{issue.status}: {issue.status_detail}] "
            else:
                tag = f"[{issue.status}] "
        else:
            tag = ""
        return f"### {issue.number}. {tag}{issue.title}"

    def all_issues(self) -> list[Issue]:
        result = []
        for _, _, issues in self.sections:
            result.extend(issues)
        return result


def slugify(title: str, max_len: int = 40) -> str:
    """Generate a branch-safe slug from an issue title."""
    slug = re.sub(r"[^a-z0-9]+", "-", title.lower()).strip("-")[:max_len].rstrip("-")
    return slug
