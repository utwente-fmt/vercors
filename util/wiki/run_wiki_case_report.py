#!/usr/bin/env python3
import argparse
import json
import os
import subprocess
import sys
from collections import Counter

import yaml


def load_manifest(path):
    with open(path, "r") as f:
        data = json.load(f)

    cases = data.get("cases")
    if not isinstance(cases, list):
        raise ValueError(f"Manifest does not contain a cases list: {path}")

    return cases


def classify_output(returncode, output, timed_out=False):
    if timed_out:
        return "Error"
    if returncode == 0:
        return "Pass"

    if returncode == 1:
        return "Fail"

    return "Error"


def run_vercors(vercors_bin, case_path, timeout_seconds):
    try:
        completed = subprocess.run(
            [vercors_bin, case_path],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            timeout=timeout_seconds,
            check=False,
        )
        return completed.returncode, completed.stdout or "", False
    except subprocess.TimeoutExpired as exc:
        output = exc.stdout or ""
        if exc.stderr:
            output += exc.stderr
        output += f"\nTimed out after {timeout_seconds} seconds."
        return None, output, True
    except FileNotFoundError as exc:
        return None, str(exc), True


def build_summary(results, counts):
    return {
        "total_cases": len(results),
        "pass": counts["pass"],
        "fail": counts["fail"],
        "error": counts["error"],
        "matched_intended": counts["matches"],
        "mismatched_intended": counts["mismatches"],
    }


def write_yaml_report(path, report):
    yaml_text = yaml.safe_dump(report, sort_keys=False, allow_unicode=True)
    with open(path, "w") as f:
        f.write(yaml_text)


def build_report(
    manifest_cases,
    cases_dir,
    vercors_bin,
    timeout_seconds,
    checkpoint_path=None,
    checkpoint_every=1,
    report_metadata=None,
):
    results = []
    counts = Counter()
    total = len(manifest_cases)
    stream = sys.stdout
    metadata = report_metadata or {}

    print(f"Running {total} wiki cases...", file=stream, flush=True)

    for index, case in enumerate(manifest_cases, start=1):
        case_name = case.get("case_name")
        file_name = case.get("file_name")
        case_path = os.path.join(cases_dir, file_name)

        print(f"[{index:>3}/{total}] {case_name}", file=stream, flush=True)

        returncode, output, timed_out = run_vercors(
            vercors_bin, case_path, timeout_seconds
        )
        actual_result = classify_output(returncode, output, timed_out)
        intended_result = case.get("intended_result")
        matched = intended_result == actual_result

        counts[actual_result.lower()] += 1
        if matched:
            counts["matches"] += 1
        else:
            counts["mismatches"] += 1

        results.append(
            {
                "case_name": case_name,
                "case_file": file_name,
                "source_file": case.get("source_file"),
                "source_line": case.get("source_line"),
                "source_kind": case.get("source_kind"),
                "intended_result": intended_result,
                "pass_on_latest": bool(case.get("pass_on_latest", False)),
                "actual_result": actual_result,
                "vercors_exit_code": returncode,
                "matched_intended": matched,
                "vercors_output": output.rstrip("\n"),
            }
        )

        run_status = "MATCH" if matched else "MISMATCH"
        print(
            f"         verdict: actual={actual_result}, intended={intended_result}, {run_status}",
            file=stream,
            flush=True,
        )

        if (
            checkpoint_path
            and checkpoint_every > 0
            and (index % checkpoint_every == 0 or index == total)
        ):
            checkpoint_report = {
                "summary": build_summary(results, counts),
                "results": results,
                "progress": {
                    "completed_cases": index,
                    "total_cases": total,
                    "done": index == total,
                },
            }
            checkpoint_report.update(metadata)
            write_yaml_report(checkpoint_path, checkpoint_report)

    return {
        "summary": build_summary(results, counts),
        "results": results,
    }


def main():
    repo_root = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
    default_vercors = os.path.join(repo_root, "bin", "vct")

    parser = argparse.ArgumentParser(
        description="Run VerCors over extracted wiki cases and write a YAML report."
    )
    parser.add_argument(
        "--cases-dir", required=True, help="Directory containing extracted case files"
    )
    parser.add_argument(
        "--manifest",
        help="Path to cases-manifest.json. Defaults to <cases-dir>/cases-manifest.json",
    )
    parser.add_argument("--output", required=True, help="YAML report path. Defaults to stdout")
    parser.add_argument(
        "--vercors-bin", default=default_vercors, help="Path to the VerCors command"
    )
    parser.add_argument(
        "--timeout-seconds", type=int, default=3600, help="Timeout per case in seconds"
    )

    args = parser.parse_args()
        
    manifest_path = args.manifest or os.path.join(args.cases_dir, "cases-manifest.json")
    manifest_cases = load_manifest(manifest_path)
    report_metadata = {
        "manifest": os.path.basename(manifest_path),
        "vercors_bin": args.vercors_bin,
        "timeout_seconds": args.timeout_seconds,
    }
    report = build_report(
        manifest_cases,
        args.cases_dir,
        args.vercors_bin,
        args.timeout_seconds,
        checkpoint_path=args.output,
        report_metadata=report_metadata,
    )
    report.update(report_metadata)

    write_yaml_report(args.output, report)


if __name__ == "__main__":
    main()
