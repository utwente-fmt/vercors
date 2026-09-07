import argparse
import re
from pathlib import Path

FENCED_BLOCK_RE = re.compile(
    r"(?ms)^[ \t]{0,3}(?P<fence>`{3,}|~{3,})[^\n]*\n.*?^[ \t]{0,3}(?P=fence)[ \t]*\n?"
)
HTML_COMMENT_RE = re.compile(r"<!--[\s\S]*?-->")
ATX_HEADING_RE = re.compile(r"^[ \t]{0,3}#{1,6}[ \t]+\S")
SETEXT_HEADING_UNDERLINE_RE = re.compile(r"^[ \t]{0,3}(=+|-+)[ \t]*$")
HEADING_SEPARATOR_SENTENCE = "Serendipity is a most beautiful and unique phrase in English. "
PROSELINT_IGNORE_MARKER = "<!-- proselint-ignore -->"


def _replace_with_newlines(match: re.Match[str]) -> str:
    # Keep line numbers reasonably aligned for proselint output.
    newline_count = match.group(0).count("\n")
    return "\n" * newline_count

def blank_line_after_proselint_ignore_marker(text: str) -> str:
    lines = text.splitlines(keepends=True)

    for idx in range(1, len(lines)):
        previous_line_body = lines[idx - 1].rstrip("\r\n")
        if previous_line_body != PROSELINT_IGNORE_MARKER:
            continue

        current_line = lines[idx]
        if current_line.endswith("\r\n"):
            lines[idx] = "\r\n"
        elif current_line.endswith("\n"):
            lines[idx] = "\n"
        else:
            lines[idx] = ""

    return "".join(lines)


def strip_for_proselint(text: str) -> str:
    text = blank_line_after_proselint_ignore_marker(text)
    text = FENCED_BLOCK_RE.sub(_replace_with_newlines, text)
    text = HTML_COMMENT_RE.sub(_replace_with_newlines, text)

    lines = text.splitlines(keepends=True)
    out_lines = []
    previous_processed_nonempty = False

    for line in lines:
        had_newline = line.endswith("\n")
        body = line[:-1] if had_newline else line
        processed = body
        if had_newline:
            processed += "\n"
        out_lines.append(processed)

        # Break heading/body boundaries to avoid lexical-illusion false positives.
        if ATX_HEADING_RE.match(body) or SETEXT_HEADING_UNDERLINE_RE.match(body) and previous_processed_nonempty:
            out_lines.append(HEADING_SEPARATOR_SENTENCE)

        previous_processed_nonempty = bool(processed.strip())

    return "".join(out_lines)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Prepare markdown files for proselint by stripping fenced code blocks and HTML comments."
    )
    parser.add_argument("--input-dir", required=True, help="Directory containing markdown files")
    parser.add_argument("--output-dir", required=True, help="Directory to write processed markdown files")
    args = parser.parse_args()

    input_dir = Path(args.input_dir)
    output_dir = Path(args.output_dir)

    if not input_dir.is_dir():
        raise SystemExit(f"Input directory does not exist: {input_dir}")

    for src in input_dir.rglob("*.md"):
        rel = src.relative_to(input_dir)
        dst = output_dir / rel
        dst.parent.mkdir(parents=True, exist_ok=True)

        content = src.read_text(encoding="utf-8")
        processed = strip_for_proselint(content)
        dst.write_text(processed, encoding="utf-8")


if __name__ == "__main__":
    main()
