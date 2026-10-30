#!/usr/bin/env python3
import argparse
import json
import sys

import yaml


RESULT_PASS = "Pass"


def load_manifest(path):
    with open(path, "r") as f:
        data = json.load(f)

    cases = data.get("cases")
    if not isinstance(cases, list):
        raise ValueError(f"Manifest does not contain a cases list: {path}")
    return cases


def load_report_results(path):
    with open(path, "r") as f:
        data = yaml.safe_load(f)

    results = data.get("results")
    if not isinstance(results, list):
        raise ValueError(f"Report does not contain a results list: {path}")

    indexed = {}
    for result in results:
        case_name = result.get("case_name")
        if case_name:
            indexed[case_name] = result
    return indexed


def source_ref(case):
    source_file = case.get("source_file") or "<unknown-source>"
    source_line = case.get("source_line")
    if isinstance(source_line, int):
        return f"{source_file}:{source_line}"
    return source_file


def append_violation(violations, case_name, source, reason, expected, actual_dev, actual_release):
    violations.append(
        {
            "case_name": case_name,
            "source": source,
            "reason": reason,
            "expected": expected,
            "actual_dev": actual_dev,
            "actual_release": actual_release,
        }
    )


def validate_cases(manifest_cases, dev_results, release_results):
    violations = []

    for case in manifest_cases:
        case_name = case.get("case_name")
        expected = case.get("intended_result")
        on_latest = bool(case.get("on_latest", False))
        source = source_ref(case)

        dev_result = dev_results.get(case_name)
        release_result = release_results.get(case_name)

        actual_dev = dev_result.get("actual_result") if dev_result else None
        actual_release = release_result.get("actual_result") if release_result else None

        if actual_dev is None:
            append_violation(
                violations,
                case_name,
                source,
                "missing_dev_result",
                expected,
                actual_dev,
                actual_release,
            )
            continue

        if actual_release is None:
            append_violation(
                violations,
                case_name,
                source,
                "missing_release_result",
                expected,
                actual_dev,
                actual_release,
            )
            continue

        if actual_dev != expected:
            append_violation(
                violations,
                case_name,
                source,
                "dev_result_mismatch",
                expected,
                actual_dev,
                actual_release,
            )

        if on_latest:
            if actual_release == expected:
                append_violation(
                    violations,
                    case_name,
                    source,
                    "on_latest_but_release_matches",
                    expected,
                    actual_dev,
                    actual_release,
                )
        else:
            if actual_release != expected:
                append_violation(
                    violations,
                    case_name,
                    source,
                    "release_result_mismatch",
                    expected,
                    actual_dev,
                    actual_release,
                )

    return violations


def markdown_summary(release_version, manifest_cases, violations):
    lines = []
    lines.append("# Wiki case compatibility checks")
    lines.append("")
    lines.append(f"- Release version checked: {release_version}")
    lines.append(f"- Total cases: {len(manifest_cases)}")
    lines.append(f"- Violations: {len(violations)}")
    lines.append("")

    if not violations:
        lines.append("All wiki case compatibility checks passed.")
        lines.append("")
        return "\n".join(lines)

    lines.append("## Violations")
    lines.append("")
    for violation in violations:
        lines.append(
            "- "
            + f"{violation['case_name']} ({violation['source']}): "
            + f"{violation['reason']}; expected={violation['expected']}, "
            + f"dev={violation['actual_dev']}, release={violation['actual_release']}"
        )

    lines.append("")
    lines.append("Rule reminders:")
    lines.append("- Dev result must match intended result for every case.")
    lines.append("- Non-PassOnLatest cases must also match intended result on release.")
    lines.append("- PassOnLatest cases must not pass on release.")
    lines.append("")
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(
        description="Validate wiki case outcomes across dev and release VerCors runs."
    )
    parser.add_argument("--manifest", required=True, help="Path to cases-manifest.json")
    parser.add_argument("--dev-report", required=True, help="Path to dev YAML report")
    parser.add_argument(
        "--release-report", required=True, help="Path to release YAML report"
    )
    parser.add_argument(
        "--release-version",
        required=True,
        help="Release version label (for summary), e.g. 2.3.0",
    )
    parser.add_argument(
        "--summary-path",
        required=True,
        help="Path to write markdown summary",
    )

    args = parser.parse_args()

    manifest_cases = load_manifest(args.manifest)
    dev_results = load_report_results(args.dev_report)
    release_results = load_report_results(args.release_report)

    violations = validate_cases(manifest_cases, dev_results, release_results)
    summary = markdown_summary(args.release_version, manifest_cases, violations)

    with open(args.summary_path, "w") as f:
        f.write(summary)

    print(summary)

    if violations:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
