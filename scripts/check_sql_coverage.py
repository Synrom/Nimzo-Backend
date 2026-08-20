#!/usr/bin/env python3
"""Fail when a SQL statement in src/*.hs was not exercised by the test suite.

The checker runs the Cabal test suite with HPC enabled, reads the generated
per-module HTML, and checks the coverage span containing each SQL statement
keyword in a Haskell string literal. Add ``-- sql-coverage-ignore`` to a source
line only when that statement is intentionally excluded from this policy.
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass, field
from html.parser import HTMLParser
from pathlib import Path
import re
import subprocess
import sys


PROJECT_ROOT = Path(__file__).resolve().parent.parent
SQL_STATEMENT = re.compile(
    r"\b(?:SELECT|INSERT|UPDATE|DELETE|WITH|CREATE|ALTER|DROP|TRUNCATE|MERGE|CALL|BEGIN|COMMIT|ROLLBACK)\b"
)
IGNORE_MARKER = "sql-coverage-ignore"


@dataclass
class HpcLine:
    text: str = ""
    coverage: list[str] = field(default_factory=list)


class HpcHtmlParser(HTMLParser):
    """Recover source text and per-character tick state from HPC markup."""

    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.class_stack: list[set[str]] = []
        self.current_line: int | None = None
        self.lines: dict[int, HpcLine] = {}

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        classes = set(dict(attrs).get("class", "").split())
        self.class_stack.append(classes)

    def handle_endtag(self, tag: str) -> None:
        if self.class_stack:
            self.class_stack.pop()

    def handle_data(self, data: str) -> None:
        active_classes = set().union(*self.class_stack) if self.class_stack else set()
        if "lineno" in active_classes:
            try:
                self.current_line = int(data.strip())
                self.lines.setdefault(self.current_line, HpcLine())
            except ValueError:
                pass
            return

        if self.current_line is None:
            return

        state = "uncovered" if "nottickedoff" in active_classes else (
            "covered" if "istickedoff" in active_classes else "neutral"
        )
        for part in data.splitlines(keepends=True):
            source_text = part.rstrip("\r\n")
            line = self.lines.setdefault(self.current_line, HpcLine())
            line.text += source_text
            line.coverage.extend([state] * len(source_text))
            if part.endswith(("\n", "\r")):
                self.current_line = None


def string_literal_mask(line: str) -> list[bool]:
    """Return which characters occur inside ordinary Haskell string literals."""

    mask = [False] * len(line)
    inside = False
    escaped = False
    for index, character in enumerate(line):
        if inside:
            mask[index] = True
            if escaped:
                escaped = False
            elif character == "\\":
                escaped = True
            elif character == '"':
                inside = False
        elif character == '"':
            inside = True
            mask[index] = True
        elif line[index:index + 2] == "--":
            break
    return mask


def sql_keyword_offsets(line: str) -> list[int]:
    if IGNORE_MARKER in line:
        return []
    literal_mask = string_literal_mask(line)
    return [
        match.start()
        for match in SQL_STATEMENT.finditer(line)
        if match.start() < len(literal_mask) and literal_mask[match.start()]
    ]


def latest_report() -> Path | None:
    reports = list(PROJECT_ROOT.glob("dist-newstyle/**/t/Tests/hpc/vanilla/html/hpc_index.html"))
    return max(reports, key=lambda path: path.stat().st_mtime) if reports else None


def module_name(source: Path) -> str:
    relative = source.relative_to(PROJECT_ROOT / "src").with_suffix("")
    return ".".join(relative.parts)


def parse_markup(path: Path) -> dict[int, HpcLine]:
    parser = HpcHtmlParser()
    parser.feed(path.read_text(encoding="utf-8"))
    return parser.lines


def run_tests() -> None:
    result = subprocess.run(
        [
            "cabal",
            "test",
            "test:Tests",
            "--enable-coverage",
            "--test-show-details=failures",
        ],
        cwd=PROJECT_ROOT,
        check=False,
    )
    if result.returncode != 0:
        raise SystemExit(result.returncode)


def main() -> int:
    argument_parser = argparse.ArgumentParser(description=__doc__)
    argument_parser.add_argument(
        "--skip-tests",
        action="store_true",
        help="reuse the newest existing Cabal HPC report",
    )
    args = argument_parser.parse_args()

    if not args.skip_tests:
        run_tests()

    report = latest_report()
    if report is None:
        print("No HPC report found. Run without --skip-tests first.", file=sys.stderr)
        return 2

    html_directory = report.parent
    markup_by_module = {
        path.name.removesuffix(".hs.html"): path
        for path in html_directory.glob("*/*.hs.html")
    }
    checked = 0
    failures: list[str] = []

    for source in sorted((PROJECT_ROOT / "src").rglob("*.hs")):
        source_lines = source.read_text(encoding="utf-8").splitlines()
        relevant = {}
        for number, source_line in enumerate(source_lines, start=1):
            offsets = sql_keyword_offsets(source_line)
            if offsets:
                relevant[number] = offsets
        if not relevant:
            continue

        module = module_name(source)
        markup_path = markup_by_module.get(module)
        hpc_lines = parse_markup(markup_path) if markup_path else {}
        relative = source.relative_to(PROJECT_ROOT)

        for number, offsets in relevant.items():
            checked += 1
            hpc_line = hpc_lines.get(number)
            uncovered = hpc_line is None or any(
                offset >= len(hpc_line.coverage) or hpc_line.coverage[offset] != "covered"
                for offset in offsets
            )
            if uncovered:
                reason = "no HPC data" if hpc_line is None else "not executed"
                failures.append(f"{relative}:{number}: {reason}: {source_lines[number - 1].strip()}")

    if failures:
        print(f"SQL coverage check failed: {len(failures)} of {checked} statement lines are uncovered.")
        for failure in failures:
            print(f"  {failure}")
        print(f"Use '-- {IGNORE_MARKER}' only for an intentional exclusion.")
        return 1

    print(f"SQL coverage check passed: all {checked} SQL statement lines were executed.")
    print(f"HPC report: {report.relative_to(PROJECT_ROOT)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
