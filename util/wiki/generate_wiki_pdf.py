#!/usr/bin/env python3
import base64
import json
import optparse
import os
import re
import subprocess
import sys
import tempfile
import time
from urllib.parse import unquote

try:
    import pypandoc
except ModuleNotFoundError:
    pypandoc = None


class SnippetTestcase:
    """
    A testcase consisting of custom snippets, e.g.:

    <!-- standaloneSnip smallCase
    //:: cases smallCase
    //:: verdict Fail
    class Test {
    void test() {
    -->

    This example will **fail**:

    <!-- codeSnip smallCase -->
    ```java
    assert false;
    ```

    <!-- standaloneSnip smallCase
    }
    }
    -->
    """

    def __init__(self):
        self.content = ""
        self.verdict = "Pass"
        self.language = None
        self.source_file = None
        self.source_line = None
        self.source_kind = None

    def add_content(self, content):
        self.content += content
        # Standalone snippets can include directives like "//:: verdict Fail".
        m = re.search(r"(?m)^\s*//::\s*verdict\s+(\w+)\s*$", self.content)
        if m and m.group(1) in {"Pass", "Fail", "Error"}:
            self.verdict = m.group(1)

    def render(self):
        return self.content

class UnknownVerdict(Exception):
    pass

class TemplateTestcase:
    """
    Testcases defined by template, e.g.:

    <!-- testBlock Fail -->
    ```java
    assert false;
    ```

    testBlock wraps the code in a method and class
    testMethod wraps the code in a class
    test returns the code as is

    testBlock and testMethod are compatible with java and pvl.
    The case name is derived from the heading structure.
    """

    METHOD = \
"""{final}class Test {{
{content}
}}"""

    BLOCK = \
"""{final}class Test {{
  void test() {{
{content}
  }}
}}"""

    BLOCK_PVL = \
"""void test() {{
{content}
}}"""

    HEADER = \
"""//:: cases {case_name}
//:: verdict {verdict}
//:: tools silicon
"""

    def __init__(self, case_name, template_kind, verdict):
        if verdict and verdict not in {"Pass", "Fail", "Error"}:
            raise UnknownVerdict()

        self.template_kind = template_kind
        self.case_name = case_name
        self.verdict = verdict if verdict else "Pass"
        self.content = None
        self.language = None
        self.source_file = None
        self.source_line = None
        self.source_kind = None

    def add_content(self, content):
        if self.content is not None:
            raise RuntimeError

        self.content = content

    def indent(self, amount, text):
        return '\n'.join("  " * amount + line for line in text.split("\n"))
    
    def render_header(self):
        return TemplateTestcase.HEADER.format(case_name=self.case_name, verdict=self.verdict)

    def render_body(self):
        if self.template_kind == 'test':
            return self.content
        elif self.template_kind == 'testMethod':
            return TemplateTestcase.METHOD.format(
                    final="final " if self.language == "java" else "",
                    content=self.indent(1, self.content)
                    )
        elif self.template_kind == 'testBlock' and self.language == "pvl":
            return TemplateTestcase.BLOCK_PVL.format(
                    content=self.indent(1, self.content)
                    )
        elif self.template_kind == 'testBlock' and self.language == "java":
            return TemplateTestcase.BLOCK.format(
                    final="final ",
                    content=self.indent(2, self.content)
                    )
        else:
            raise RuntimeError()

    def render(self):
        return self.render_header() + self.render_body()

measurement_time = 0
def start_measuring_time():
    global measurement_time
    measurement_time = time.perf_counter()

def print_elapsed_time():
    global measurement_time
    print(f" Done ({time.perf_counter() - measurement_time:.2f}s)")


def ensure_pypandoc_available():
    """
    Ensure pypandoc is installed before continuing.
    """
    if pypandoc is not None:
        return

    print("Error: missing Python dependency 'pypandoc'.", file=sys.stderr)
    print("Install it with one of:", file=sys.stderr)
    print("  pip install -r util/wiki/requirements.txt", file=sys.stderr)
    print("  pip install pypandoc_binary", file=sys.stderr)
    sys.exit(2)


def ask_yes_no(question, default=False):
    """
    Ask the user a yes/no question and return True for yes.
    """
    suffix = "[Y/n]" if default else "[y/N]"

    while True:
        answer = input(f"{question} {suffix} ").strip().lower()
        if not answer:
            return default
        if answer in {"y", "yes"}:
            return True
        if answer in {"n", "no"}:
            return False
        print("Please answer yes or no.")


def ensure_pandoc_available():
    """
    Ensure pypandoc resolves a usable pandoc binary.
    """
    try:
        version = pypandoc.get_pandoc_version()
    except OSError:
        print("Pandoc was not found for pypandoc.")

        if not sys.stdin.isatty():
            print("Cannot prompt in non-interactive mode.", file=sys.stderr)
            print("Install pandoc manually (e.g. 'sudo apt install pandoc')", file=sys.stderr)
            print("or re-run interactively to allow automatic download.", file=sys.stderr)
            sys.exit(2)

        should_download = ask_yes_no("Download pandoc now via pypandoc?", default=False)
        if not should_download:
            print("Pandoc installation skipped.", file=sys.stderr)
            print("Install pandoc manually (e.g. 'sudo apt install pandoc' or in a python terminal with 'pypandoc.download_pandoc()')", file=sys.stderr)
            print("and run this script again.", file=sys.stderr)
            sys.exit(2)

        print("Downloading pandoc...", end="")
        start_measuring_time()
        try:
            pypandoc.download_pandoc()
        except Exception as e:  # noqa: BLE001
            print_elapsed_time()
            print(f"Failed to download pandoc automatically: {e}", file=sys.stderr)
            print("Install pandoc manually (e.g. 'sudo apt install pandoc' or in a python terminal with 'pypandoc.download_pandoc()')", file=sys.stderr)
            print("or provide PYPANDOC_PANDOC with a valid pandoc path.", file=sys.stderr)
            sys.exit(2)
        print_elapsed_time()
        version = pypandoc.get_pandoc_version()

    get_path = getattr(pypandoc, "get_pandoc_path", None)
    if callable(get_path):
        try:
            pandoc_path = get_path()
        except Exception:  # noqa: BLE001
            pandoc_path = "<unknown>"
    else:
        pandoc_path = os.environ.get("PYPANDOC_PANDOC", "<auto>")

    print(f"Using pandoc via pypandoc: {version} ({pandoc_path})")

def collect_chapters(wiki_location):
    """
    Parse chapter markdown files once using commonmark+sourcepos and merge into one document.
    """
    chapters = load_chapter_entries(wiki_location)

    print("Parsing chapters...", end="")
    start_measuring_time()
    chapter_docs = []
    pandoc_version = None
    for name, file_name in chapters:
        with open(os.path.join(wiki_location, file_name + ".md"), "r") as f:
            markdown_text = f.read()

        parsed = json.loads(pypandoc.convert_text(markdown_text, "json", "commonmark+sourcepos"))
        if pandoc_version is None:
            pandoc_version = parsed["pandoc-api-version"]

        shift_header_levels(parsed["blocks"], 2)
        attach_source_file(parsed["blocks"], file_name + ".md")

        chapter_docs.append({
            "name": name,
            "file_name": file_name + ".md",
            "blocks": parsed["blocks"],
        })
    print_elapsed_time()

    print("Merging chapters...", end="")
    start_measuring_time()
    merged_blocks = []
    for chapter in chapter_docs:
        merged_blocks.append(make_header_block(chapter["name"]))
        merged_blocks.extend(chapter["blocks"])
    print_elapsed_time()

    return {
        "chapters": chapter_docs,
        "document": {
            "blocks": merged_blocks,
            "pandoc-api-version": pandoc_version,
            "meta": {},
        },
    }


def load_chapter_entries(wiki_location):
    with open(os.path.join(wiki_location, "_Sidebar.md"), "r") as f:
        contents = f.read()

    contents = unquote(contents)

    any_re = re.compile(r"\[(.+)\]\(https.*\/(.+)\)")
    chapter_re = re.compile(r"\[([-A-Za-z \/\?\&,]+)\]\(https.*\/([-A-Za-z\?\&,]+)\)")
    chapters = []
    for line in contents.splitlines():
        m = chapter_re.search(line)
        if m:
            chapters.append(m.groups())
        elif any_re.search(line):
            print(f"Warning: sidebar entry did not match our chapter regex and is not included: {line.strip()}", file=sys.stderr)

    return [chapter for chapter in chapters if chapter[0] != "Home"]


def heading_slug(text):
    slug = re.sub(r"[^a-z0-9\- ]+", "", text.lower()).strip().replace(" ", "-")
    return slug or "chapter"


def text_to_inlines(text):
    parts = []
    words = text.split(" ")
    for idx, word in enumerate(words):
        if word:
            parts.append({"t": "Str", "c": word})
        if idx != len(words) - 1:
            parts.append({"t": "Space"})
    return parts


def make_header_block(title):
    return {
        "t": "Header",
        "c": [1, [heading_slug(title), [], []], text_to_inlines(title)],
    }


def shift_header_levels(blocks, amount):
    for block in blocks:
        if block.get("t") == "Header":
            block["c"][0] += amount
        if block.get("t") == "Div":
            shift_header_levels(block["c"][1], amount)


def attach_source_file(blocks, source_file):
    for block in blocks:
        block["_source_file"] = source_file
        if block.get("t") == "Div":
            attach_source_file(block["c"][1], source_file)


def extract_line_from_data_pos(data_pos):
    if not data_pos:
        return None

    # Examples: "12:1-12:20", "8:3-10:5"
    m = re.match(r"^(\d+):\d+", data_pos)
    if not m:
        return None
    return int(m.group(1))


def attrs_data_pos(attrs):
    # attrs are [identifier, classes, [[k,v], ...]]
    if not isinstance(attrs, list) or len(attrs) < 3:
        return None

    for pair in attrs[2]:
        if len(pair) == 2 and pair[0] == "data-pos":
            return pair[1]

    return None


def block_data_pos(block):
    t = block.get("t")
    c = block.get("c")

    if t == "Header":
        return attrs_data_pos(c[1])
    if t == "CodeBlock":
        return attrs_data_pos(c[0])
    if t == "RawBlock" and isinstance(c, list) and len(c) > 2:
        return attrs_data_pos(c[2])
    if t == "Div":
        return attrs_data_pos(c[0])
    return None


def inline_text(inlines):
    parts = []

    def append_from_nested(value):
        if isinstance(value, list):
            parts.append(inline_text(value))

    for inline in inlines:
        t = inline.get("t")
        if t == "Str":
            parts.append(inline.get("c", ""))
        elif t == "Space":
            parts.append(" ")
        elif t in {"SoftBreak", "LineBreak"}:
            parts.append("\n")
        elif t == "Code":
            parts.append(inline.get("c", [None, ""])[1])
        elif t in {"Emph", "Strong", "Strikeout", "Superscript", "Subscript", "SmallCaps", "Underline"}:
            append_from_nested(inline.get("c"))
        elif t in {"Quoted", "Cite", "Link", "Image", "Span"}:
            c = inline.get("c", [])
            if len(c) > 1:
                append_from_nested(c[1])
    return "".join(parts)


def block_comment_text(block):
    if block["t"] == "RawBlock" and block["c"][0] == "html":
        content = block["c"][1].strip()
        if content.startswith("<!--") and content.endswith("-->"):
            return content

    return None


def header_block_slug(block):
    """
    Return a stable slug for a pandoc Header block.
    Prefer pandoc's identifier when present; otherwise derive from heading text.
    """
    identifier = block["c"][1][0]
    if identifier:
        return identifier

    text = inline_text(block["c"][2]).strip()
    return heading_slug(text)


def iter_blocks_with_line(blocks, inherited_line=None):
    for block in blocks:
        pos_line = extract_line_from_data_pos(block_data_pos(block))
        line = pos_line if pos_line is not None else inherited_line

        if block.get("t") == "Div":
            # sourcepos can wrap elements in Div to carry data-pos, propagate this line.
            for nested in iter_blocks_with_line(block["c"][1], line):
                yield nested
            continue

        yield block, line


def collect_testcases(document, cases):
    """
    Walk the sourcepos-annotated blocks once and collect test cases with source metadata.
    """
    breadcrumbs = []
    testcase_number = 1
    code_block_label = None
    code_block_kind = None

    def set_case_source(case, source_file, source_line, source_kind):
        if case.source_file is None:
            case.source_file = source_file
            case.source_line = source_line
            case.source_kind = source_kind

    for block, line_number in iter_blocks_with_line(document['blocks']):
        source_file = block.get("_source_file")

        # Code blocks preceded by a label are added to the labeled testcase
        if block['t'] == 'CodeBlock' and code_block_label is not None:
            code_txt = block['c'][1]
            cases[code_block_label].add_content(code_txt)

            languages = block['c'][0][1]
            if len(languages) == 0:
                print(f"Error: language was not specified for code block.\nLabel: {code_block_label}\nText in code block:\n{code_txt}")
                sys.exit(1)

            cases[code_block_label].language = languages[0]
            block['_case_label'] = code_block_label
            set_case_source(cases[code_block_label], source_file, line_number, code_block_kind or 'code_block')

        code_block_label = None
        code_block_kind = None

        # Headers are put into the breadcrumbs for template testcases
        if block['t'] == 'Header':
            # if the breadcrumbs are [Heading, Section, Subsection]
            # and we have a new section "Section 2"
            # the breadcrumbs should be [Heading, Section 2]
            breadcrumbs = breadcrumbs[:block['c'][0]]
            breadcrumbs += ['?'] * (block['c'][0] - len(breadcrumbs))
            breadcrumbs[block['c'][0] - 1] = header_block_slug(block)
            testcase_number = 1

        # HTML comments that start with known directives are processed.
        content = block_comment_text(block)
        if content is not None:
            lines = [line.strip() for line in content[4:-3].strip().split('\n')]
            if lines and lines[0]:
                kind, *args = lines[0].split(' ')

                # Template label
                if kind in {'testBlock', 'testMethod', 'test'}:
                    code_block_label = '-'.join(breadcrumbs) + '-' + str(testcase_number)
                    testcase_number += 1
                    cases[code_block_label] = TemplateTestcase(code_block_label, kind, args[0] if args else 'Pass')
                    code_block_kind = kind

                # Snippet
                if kind == 'standaloneSnip':
                    label = breadcrumbs[0] + '-' + args[0]

                    if label not in cases:
                        cases[label] = SnippetTestcase()

                    set_case_source(cases[label], source_file, line_number, kind)

                    cases[label].add_content('\n'.join(lines[1:]) + '\n')

                # Snippet label for code block
                if kind == 'codeSnip':
                    code_block_label = breadcrumbs[0] + '-' + args[0]
                    code_block_kind = kind

                    if code_block_label not in cases:
                        cases[code_block_label] = SnippetTestcase()


def convert_block_php(block, cases):
    """
    If a code block has been collected into a test case, it is instead emitted as a runnable example on the website.
    """
    if block['t'] == 'CodeBlock' and '_case_label' in block:
        code_here_data = base64.b64encode(block['c'][1].encode('utf-8')).decode('utf-8')
        case = cases[block['_case_label']]
        data = base64.b64encode(case.render().encode('utf-8')).decode('utf-8')
        return {
            't': 'RawBlock',
            'c': ['html',
                  f"<?= VerificationWidget::widget(['initialLanguage' => '{case.language}', 'initialCode' => base64_decode('{data}'), 'hide' => true, 'initialCodeOnHide' => base64_decode('{code_here_data}') ]) ?>"],
        }
    else:
        return block


def output_php(path, blocks, cases, version):
    blocks = [{
        't': 'RawBlock',
        'c': ['html', "<?php use app\\components\\VerificationWidget; ?>\n"]
    }] + [convert_block_php(block, cases) for block in blocks]

    wiki_text = json.dumps({
        'blocks': blocks,
        'pandoc-api-version': version,
        'meta': {},
    })

    pypandoc.convert_text(
        wiki_text,
        "html",
        format="json",
        outputfile=path)


def convert_block_jinja(block, cases):
    if block['t'] == 'CodeBlock' and '_case_label' in block:
        initial_data = repr(block['c'][1])
        case = cases[block['_case_label']]
        data = repr(case.render())

        invocation = f'verification_editor({data}, languages, initial_language={repr(case.language)}, start_hidden=True, initial_hidden_code={initial_data})'

        return {
            't': 'RawBlock',
            'c': [
                'html',
                '{{ ' + invocation + ' }}',
            ]
        }
    else:
        return block


def output_jinja(path, blocks, cases, version):
    blocks = [{
        't': 'RawBlock',
        'c': [
            'html',
            "{% from 'verification_editor.html' import verification_editor %}",
        ]
    }] + [convert_block_jinja(block, cases) for block in blocks]

    wiki_text = json.dumps({
        'blocks': blocks,
        'pandoc-api-version': version,
        'meta': {},
    })

    pypandoc.convert_text(wiki_text, "html", format="json", outputfile=path)


def get_html(elements):
    result = ""

    for element in elements:
        if element['t'] == 'Str':
            result += element['c']
        elif element['t'] == 'Space':
            result += ' '
        elif element['t'] == 'Code':
            result += '<code>' + element['c'][1] + '</code>'
        else:
            assert False, f"Unrecognized element type for HTML header: {element['t']} in block {element}"

    return result


def output_menu(path, blocks, version):
    l1_target = []
    l2_target = None

    for block in blocks:
        if block['t'] == 'Header':
            level = block['c'][0]
            if level == 1:
                l2_target = []
                l1_target.append((get_html(block['c'][2]), block['c'][1][0], l2_target))
            elif level == 2:
                l2_target.append((get_html(block['c'][2]), block['c'][1][0]))

    with open(path, "w") as f:
        f.write("<ul>\n")
        for header, name, subheaders in l1_target:
            f.write("<li>\n")
            f.write(f'<a href="#{name}">')
            f.write(header + "</a>\n")
            if subheaders:
                f.write("<ul>\n")
                for subheader, name in subheaders:
                    f.write(f'<li><a href="#{name}">')
                    f.write(subheader + "</a></li>\n")
                f.write("</ul>\n")
            f.write("</li>\n")
        f.write("</ul>\n")

def shared_pandoc_opts(generate_toc):
    return ((["--toc", "--toc-depth", "2"] if generate_toc else [])
        + [ "--metadata", "title=VerCors Tutorial" ])

def output_pdf(path, blocks, version, generate_toc=True):
    wiki_text = json.dumps({
        'blocks': blocks,
        'pandoc-api-version': version,
        'meta': {},
    })

    pypandoc.convert_text(
        wiki_text,
        "pdf",
        format="json",
        outputfile=path,
        extra_args=shared_pandoc_opts(generate_toc) + ["--pdf-engine=xelatex"])

def output_html(path, blocks, version, generate_toc=True):
    wiki_text = json.dumps({
        'blocks': blocks,
        'pandoc-api-version': version,
        'meta': {}
    })

    header_includes = """
    <style>
        body {
            max-width: 50em;
            margin: 0 auto;
        }
    </style>
    """

    pypandoc.convert_text(
        wiki_text,
        "html",
        format="json",
        outputfile=path,
        extra_args=["-s", "-V", f"header-includes={header_includes}"] + shared_pandoc_opts(generate_toc))

class UnknownLanguageError(Exception):
    pass

class CasesExtractionFailed(Exception):
    pass

class CaseWithoutTool(Exception):
    pass

def language_to_extension(language):
    # Ok, this looks a bit stupid, but we cannot assume the "language" attribute github uses for markdown code snippets will never diverge from extensions used for files of that type...
    language = language.strip().lower()
    if language == "java":
        return "java"
    elif language == "c" or language == "opencl":
        return "c"
    elif language == "pvl":
        return "pvl"
    elif language == "cuda":
        return "cu"
    else:
        raise UnknownLanguageError

def output_cases(path, cases):
    os.makedirs(path, exist_ok=True)

    ok = 0
    not_ok = 0
    manifest = []

    for case_name in cases:
        case = cases[case_name]
        try:
            p = os.path.join(path, f"{case_name}.{language_to_extension(case.language)}")
            with open(p, "w") as f:
                f.write(case.render())
            ok += 1

            manifest.append({
                "case_name": case_name,
                "file_name": os.path.basename(p),
                "language": case.language,
                "intended_result": case.verdict,
                "source_file": case.source_file,
                "source_line": case.source_line,
                "source_kind": case.source_kind,
            })
        except UnknownLanguageError:
            print(f"Unknown language {case.language} in case {case_name}")
            not_ok += 1

    print(f"Extracted {ok} cases successfully. {not_ok} cases failed.")

    with open(os.path.join(path, "cases-manifest.json"), "w") as f:
        json.dump({"cases": manifest}, f, indent=2)

    if not_ok > 0:
        raise CasesExtractionFailed

if __name__ == "__main__":
    ensure_pypandoc_available()
    ensure_pandoc_available()

    parser = optparse.OptionParser()
    parser.add_option('-i', '--input', dest='source_path', help='directory where the wiki is stored', metavar='FILE')
    parser.add_option('-w', '--php', dest='php_path', help='write wiki to php file for the website', metavar='FILE')
    parser.add_option('-j', '--jinja', dest='jinja_path', help='write wiki to jinja template for the website', metavar='FILE')
    parser.add_option('-m', '--menu', dest='menu_path', help='extract a menu for the website', metavar='FILE')
    parser.add_option('-p', '--pdf', dest='pdf_path', help='write wiki to a latex-typeset pdf', metavar='FILE')
    parser.add_option('--html', dest='html_path', help='write wiki to an html file', metavar='FILE')
    parser.add_option('-c', '--cases', dest='cases_path', help='write test cases extracted from the wiki to a folder')


    options, args = parser.parse_args()

    if not any([options.php_path, options.jinja_path, options.menu_path, options.pdf_path, options.html_path, options.cases_path]):
        parser.error("No output type: please set one or more of the output paths. (try --help)")

    if options.source_path:
        source_path = options.source_path
    else:
        path = tempfile.mkdtemp()
        subprocess.run(["git", "clone", "https://github.com/utwente-fmt/vercors.wiki.git"], cwd=path, check=True)
        source_path = os.path.join(path, "vercors.wiki")

    chapter_data = collect_chapters(source_path)
    document = chapter_data["document"]
    pandoc_version = document['pandoc-api-version']
    cases = {}

    print("Collecting test cases...")
    collect_testcases(document, cases)

    blocks = document['blocks']

    if options.php_path:
        print("Creating PHP...")
        output_php(options.php_path, blocks, cases, pandoc_version)

    if options.jinja_path:
        print("Creating jinja template...")
        output_jinja(options.jinja_path, blocks, cases, pandoc_version)

    if options.menu_path:
        print("Creating menu...")
        output_menu(options.menu_path, blocks, pandoc_version)

    if options.pdf_path:
        print("Creating PDF...")
        output_pdf(options.pdf_path, blocks, pandoc_version)

    if options.html_path:
        print("Creating HTML...")
        output_html(options.html_path, blocks, pandoc_version)

    if options.cases_path:
        print("Creating wiki test suite...")
        output_cases(options.cases_path, cases)
        print("done")
