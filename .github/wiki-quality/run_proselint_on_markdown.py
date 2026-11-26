import argparse
import sys
from pathlib import Path

from prepare_proselint_input import strip_for_proselint
from proselint.checks import __register__
from proselint.config import load_from
from proselint.registry import CheckRegistry
from proselint.tools import LintFile


def to_display_path(rel_path: Path, prefix: str) -> str:
    rel = rel_path.as_posix()
    if not prefix:
        return rel
    return f"{prefix.rstrip('/')}/{rel}"


def run_proselint_for_file(source_file: Path, rel_path: Path, config, prefix: str) -> int:
    processed = strip_for_proselint(source_file.read_text(encoding="utf-8"))
    display_path = to_display_path(rel_path, prefix)

    suggestions = LintFile(display_path, processed).lint(config)
    for suggestion in suggestions:
        line, col = suggestion.pos
        check_path = suggestion.check_result.check_path
        message = suggestion.check_result.message
        print(f"{display_path}:{line}:{col}: {check_path}: {message}")

    return 1 if suggestions else 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Preprocess markdown files and run proselint while preserving source file paths in output."
    )
    parser.add_argument("--input-dir", required=True, help="Directory containing markdown files")
    parser.add_argument("--proselint-config", required=True, help="Path to proselint config JSON")
    parser.add_argument(
        "--path-prefix",
        default="",
        help="Prefix to use in displayed file paths (for example: /wiki)",
    )
    args = parser.parse_args()

    input_dir = Path(args.input_dir)
    config_path = Path(args.proselint_config)

    if not input_dir.is_dir():
        print(f"Input directory does not exist: {input_dir}", file=sys.stderr)
        return 2
    if not config_path.is_file():
        print(f"Proselint config file does not exist: {config_path}", file=sys.stderr)
        return 2

    CheckRegistry().register_many(__register__)
    config = load_from(config_path)

    exit_code = 0
    files = sorted(input_dir.rglob("*.md"))
    for source_file in files:
        rel_path = source_file.relative_to(input_dir)
        code = run_proselint_for_file(source_file, rel_path, config, args.path_prefix)
        exit_code = max(exit_code, code)

    return exit_code


if __name__ == "__main__":
    raise SystemExit(main())
