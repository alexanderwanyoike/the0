"""Tests for the audit file parser and mutator."""

import pytest
from tech_debt_agents.audit_file import AuditFile, Issue, slugify


class TestParsing:
    def test_parse_sections(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        section_names = [name for name, _, _ in af.sections]
        assert section_names == ["CRITICAL/HIGH", "MEDIUM", "LOW"]

    def test_parse_issue_count(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        all_issues = af.all_issues()
        assert len(all_issues) == 9

    def test_parse_issue_numbers(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        numbers = [i.number for i in af.all_issues()]
        assert numbers == [1, 2, 3, 12, 13, 14, 24, 25, 26]

    def test_parse_severity(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        issues = {i.number: i for i in af.all_issues()}
        assert issues[1].severity == "CRITICAL/HIGH"
        assert issues[12].severity == "MEDIUM"
        assert issues[24].severity == "LOW"

    def test_parse_resolved_status(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        issues = {i.number: i for i in af.all_issues()}
        assert issues[1].status == "RESOLVED"
        assert issues[1].status_detail == ""

    def test_parse_in_progress_status(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        issues = {i.number: i for i in af.all_issues()}
        assert issues[13].status == "IN-PROGRESS"
        assert issues[13].status_detail == "fix/audit-13-excessive-any"

    def test_parse_attempted_status(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        issues = {i.number: i for i in af.all_issues()}
        assert issues[25].status == "ATTEMPTED"
        assert issues[25].status_detail == "build-failure 2026-03-12"

    def test_parse_no_status(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        issues = {i.number: i for i in af.all_issues()}
        assert issues[2].status is None
        assert issues[12].status is None

    def test_parse_title_with_backticks(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        issues = {i.number: i for i in af.all_issues()}
        assert issues[12].title == "Unsafe `(request as any).user?.uid` pattern repeated everywhere"

    def test_parse_body_lines(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        issues = {i.number: i for i in af.all_issues()}
        body_text = "".join(issues[12].body)
        assert "custom-bot.controller.ts" in body_text
        assert "@CurrentUser()" in body_text


class TestWriteRoundtrip:
    def test_roundtrip_preserves_content(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        output = af.to_text()
        # Parse again and compare structure
        af2 = AuditFile.parse_text(output)
        assert len(af.all_issues()) == len(af2.all_issues())
        for i1, i2 in zip(af.all_issues(), af2.all_issues()):
            assert i1.number == i2.number
            assert i1.title == i2.title
            assert i1.status == i2.status
            assert i1.severity == i2.severity


class TestStatusMutation:
    def test_mark_in_progress(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        issue = next(i for i in af.all_issues() if i.number == 12)
        af.mark_in_progress(issue, "fix/audit-12-unsafe-request")
        assert issue.status == "IN-PROGRESS"
        assert issue.status_detail == "fix/audit-12-unsafe-request"
        # Verify it renders correctly
        output = af.to_text()
        assert "### 12. [IN-PROGRESS: fix/audit-12-unsafe-request]" in output

    def test_mark_attempted(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        issue = next(i for i in af.all_issues() if i.number == 13)
        af.mark_attempted(issue, "build-failure 2026-03-13")
        assert issue.status == "ATTEMPTED"
        output = af.to_text()
        assert "[ATTEMPTED: build-failure 2026-03-13]" in output

    def test_mark_resolved(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        issue = next(i for i in af.all_issues() if i.number == 2)
        af.mark_resolved(issue)
        output = af.to_text()
        assert "### 2. [RESOLVED] API Dockerfile" in output


class TestIssueSelection:
    def test_picks_first_medium_without_status(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        issue = af.pick_eligible_issue()
        assert issue is not None
        assert issue.number == 12
        assert issue.severity == "MEDIUM"

    def test_skips_resolved(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        issue = af.pick_eligible_issue()
        assert issue.number != 1  # Issue 1 is RESOLVED

    def test_skips_in_progress(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        issue = af.pick_eligible_issue()
        assert issue.number != 13  # Issue 13 is IN-PROGRESS

    def test_skips_critical_high(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        issue = af.pick_eligible_issue()
        # Should never pick issues 1-3 (CRITICAL/HIGH)
        assert issue.number not in (1, 2, 3)

    def test_falls_through_to_low(self, sample_audit_text):
        """When all MEDIUM issues have status tags, should pick first eligible LOW."""
        af = AuditFile.parse_text(sample_audit_text)
        # Mark all MEDIUM issues
        for i in af.all_issues():
            if i.severity == "MEDIUM" and i.status is None:
                af.mark_resolved(i)

        issue = af.pick_eligible_issue()
        assert issue is not None
        assert issue.severity == "LOW"
        assert issue.number == 24  # First LOW without status

    def test_returns_none_when_all_done(self, sample_audit_text):
        af = AuditFile.parse_text(sample_audit_text)
        for i in af.all_issues():
            if i.status is None:
                af.mark_resolved(i)
        assert af.pick_eligible_issue() is None


class TestSlugify:
    def test_basic(self):
        assert slugify("Hello World") == "hello-world"

    def test_special_chars(self):
        assert slugify("Unsafe `(request as any).user?.uid` pattern") == "unsafe-request-as-any-user-uid-pattern"

    def test_truncation(self):
        slug = slugify("A" * 100)
        assert len(slug) <= 40

    def test_no_trailing_dash(self):
        slug = slugify("hello world and more things to say here!!", max_len=20)
        assert not slug.endswith("-")

    def test_slashes(self):
        assert slugify("No `/health` or `/readiness` endpoint") == "no-health-or-readiness-endpoint"
