#!/usr/bin/env python3
from html import escape
import re
import shutil
import subprocess
import tempfile
from urllib.parse import unquote
import pypandoc
import json
import os
import base64
import optparse
import sys

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
        self.language = None

    def add_content(self, content):
        self.content += content

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

    HEADER = \
"""//:: cases {case_name}
//:: verdict {verdict}
//:: tools silicon
"""

    def __init__(self, case_name, template_kind, verdict):
        if verdict:
            if not (verdict == "Pass" or verdict == "Fail" or verdict == "Error"):
                raise UnknownVerdict()

        self.template_kind = template_kind
        self.case_name = case_name
        self.verdict = verdict if verdict else "Pass"
        self.content = None
        self.language = None

    def add_content(self, content):
        if self.content is not None:
            raise RuntimeError

        self.content = content

    def indent(self, amount, text):
        return '\n'.join("    " * amount + line for line in text.split("\n"))
    
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
        elif self.template_kind == 'testBlock':
            return TemplateTestcase.BLOCK.format(
                    final="final " if self.language == "java" else "",
                    content=self.indent(2, self.content)
                    )
        else:
            raise RuntimeError()

    def render(self):
        return self.render_header() + self.render_body()

def slugify(text):
    slug = re.sub(r"[^a-z0-9]+", "-", text.lower()).strip("-")
    return slug or "section"

def new_sidebar_node(kind, title, children=None, file_name=None):
    return {
        'kind': kind,
        'title': title,
        'children': [] if children is None else children,
        'file_name': file_name,
    }

def parse_sidebar(contents):
    chapter_heading_re = re.compile(r"^\*\*(.+?)\*\*\s*$")
    list_item_re = re.compile(r"^(?P<indent>\s*)\*\s+(?P<body>.+?)\s*$")
    any_re = re.compile(r"\[(.+?)\]\(https.*\/(.+?)\)")
    chapter_re = re.compile(r"\[([^\]]+)\]\(https.*\/([^)]+)\)")

    chapters = []
    pages = []
    current_chapter = None
    stack = []

    for raw_line in contents.splitlines():
        line = raw_line.rstrip()
        stripped = line.strip()
        if not stripped:
            continue

        chapter_heading_match = chapter_heading_re.match(stripped)
        if chapter_heading_match:
            heading_title = chapter_heading_match.group(1)
            heading_link_match = chapter_re.fullmatch(heading_title)
            if heading_link_match:
                link_title, _ = heading_link_match.groups()
                if link_title == 'Home':
                    current_chapter = None
                    stack = []
                    continue
                heading_title = link_title

            current_chapter = new_sidebar_node('chapter', heading_title)
            chapters.append(current_chapter)
            stack = [(-1, current_chapter)]
            continue

        list_item_match = list_item_re.match(line)
        if not list_item_match:
            if any_re.search(line):
                print(f"Warning: sidebar entry did not match chapter_re and is not included: {stripped}", file=sys.stderr)
            continue

        if current_chapter is None:
            continue

        indent = len(list_item_match.group('indent').replace('\t', '  '))
        body = list_item_match.group('body').strip()
        link_match = chapter_re.search(body)

        if link_match:
            title, file_name = link_match.groups()
            node = new_sidebar_node('page', title, file_name=file_name)
        else:
            if any_re.search(body):
                print(f"Warning: sidebar entry did not match chapter_re and is not included: {body}", file=sys.stderr)
                continue
            node = new_sidebar_node('group', body)

        while stack and indent <= stack[-1][0]:
            stack.pop()

        parent = stack[-1][1]
        parent['children'].append(node)
        stack.append((indent, node))

        if node['kind'] == 'page' and node['title'] != 'Home':
            pages.append(node)

    return {
        'chapters': chapters,
        'pages': pages,
    }

def load_sidebar(wiki_location):
    with open(os.path.join(wiki_location, "_Sidebar.md"), "r") as f:
        contents = f.read()

    contents = unquote(contents)
    sidebar = parse_sidebar(contents)
    return sidebar

def collect_testcases(document, cases):
    """
    Walks through the blocks of the document and collects test cases as described in SnippetTestcase and TemplateTestcase
    """
    breadcrumbs = []
    testcase_number = 1
    code_block_label = None

    for block in document['blocks']:
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

        code_block_label = None

        # Headers are put into the breadcrumbs for template testcases
        if block['t'] == 'Header':
            # if the breadcrumbs are [Heading, Section, Subsection]
            # and we have a new section "Section 2"
            # the breadcrumbs should be [Heading, Section 2]
            breadcrumbs = breadcrumbs[:block['c'][0]]
            breadcrumbs += ['wiki'] * (block['c'][0] - len(breadcrumbs))
            header_id = block['c'][1][0]
            if not header_id:
                header_text = []
                for element in block['c'][2]:
                    if element['t'] == 'Str':
                        header_text.append(element['c'])
                    elif element['t'] == 'Space':
                        header_text.append(' ')
                    elif element['t'] == 'Code':
                        header_text.append(element['c'][1])
                header_id = slugify(''.join(header_text)) if header_text else 'section'
            breadcrumbs[block['c'][0] - 1] = header_id
            testcase_number = 1

        # Raw blocks that are comments starting with something we recognize are processed
        if block['t'] == 'RawBlock' and block['c'][0] == 'html':
            content = block['c'][1].strip()
            if content.startswith('<!--') and content.endswith('-->'):
                lines = [line.strip() for line in content[4:-3].strip().split('\n')]
                kind, *args = lines[0].split(' ')

                # Template label
                if kind in {'testBlock', 'testMethod', 'test'}:
                    base_label = '-'.join(breadcrumbs) if breadcrumbs else 'wiki'
                    code_block_label = base_label + '-' + str(testcase_number)
                    testcase_number += 1
                    cases[code_block_label] = TemplateTestcase(code_block_label, kind, args[0] if args else 'Pass')

                # Snippet
                if kind == 'standaloneSnip':
                    label_prefix = breadcrumbs[0] if breadcrumbs else 'wiki'
                    label = label_prefix + '-' + args[0]

                    if label not in cases:
                        cases[label] = SnippetTestcase()

                    cases[label].add_content('\n'.join(lines[1:]) + '\n')

                # Snippet label for code block
                if kind == 'codeSnip':
                    label_prefix = breadcrumbs[0] if breadcrumbs else 'wiki'
                    code_block_label = label_prefix + '-' + args[0]

                    if code_block_label not in cases:
                        cases[code_block_label] = SnippetTestcase()

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

class UnknownLanguageError(Exception):
    pass

class CasesExtractionFailed(Exception):
    pass

def language_to_extension(language):
    # Ok, this looks a bit stupid, but we cannot assume the "language" attribute github uses for markdown code snippets will never diverge from extensions used for files of that type...
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

    for case_name in cases:
        case = cases[case_name]
        try:
            p = os.path.join(path, f"{case_name}.{language_to_extension(case.language)}")
            content = case.render()
            with open(p, "w") as f:
                f.write(content)
            ok += 1
        except UnknownLanguageError:
            print(f"Unknown language {case.language} in case {case_name}")
            not_ok += 1

    print(f"Extracted {ok} cases successfully. {not_ok} cases failed.")

    if not_ok > 0:
        raise CasesExtractionFailed
    
def render_verification_editor_html(initial_code, initial_hidden_code, language_extension, language_label, template_kind=None, case_name=None, verdict='Pass'):
    def html_text_no_markdown_breaks(text):
    # Keep raw HTML blocks stable in mdBook markdown by avoiding literal blank lines.
        return escape(text).replace('\r\n', '\n').replace('\r', '\n').replace('\n', '&#10;')

    hidden_code = html_text_no_markdown_breaks(initial_hidden_code)
    full_code_base64 = base64.b64encode(initial_code.encode('utf-8')).decode('ascii')
    template_kind_attr = escape(template_kind or '', quote=True)
    case_name_attr = escape(case_name or '', quote=True)
    verdict_attr = escape(verdict or 'Pass', quote=True)
    language_extension_attr = escape(language_extension, quote=True)
    language_label_attr = escape(language_label, quote=True)

    return (
        f'<div class="verification-container" data-examplecode-b64="{full_code_base64}" data-template-kind="{template_kind_attr}" data-case-name="{case_name_attr}" data-case-verdict="{verdict_attr}" data-language-ext="{language_extension_attr}" data-language-label="{language_label_attr}">'
        '<pre style="margin-bottom: 0" class="verification-text playground">'
        f'<code class="language-{escape(language_extension)} no_run editable">{hidden_code}</code>'
        '<div class="buttons">'
        '<button class="fa fa-play code-run-button" title="Run this code" aria-label="Run this code"></button>'
        '</div></pre>'
        f'<div class="verification-language" style="background-color: #dddddd; padding: 0.4ex 1ex">Language for VerCors: <strong>{escape(language_label)}</strong></div>'
        '<div class="verification-progress verification-non-plain" style="display: none; background-color: #dddddd; padding: 0.4ex 1ex">'
        '<span class="fa"></span>'
        '<span class="verification-progress-text"></span>'
        '</div>'
        '<pre class="verification-log verification-non-plain" style="display: none"></pre>'
        '</div>'
    )

def convert_block_mdbook(block, cases):
    if block['t'] == 'CodeBlock' and '_case_label' in block:
        case = cases[block['_case_label']]
        language_extension = language_to_extension(case.language)
        template_kind = case.template_kind if isinstance(case, TemplateTestcase) else None
        case_name = case.case_name if isinstance(case, TemplateTestcase) else None
        verdict = case.verdict if isinstance(case, TemplateTestcase) else 'Pass'
        return {
            't': 'RawBlock',
            'c': ['html', render_verification_editor_html(case.render(), block['c'][1], language_extension, case.language, template_kind, case_name, verdict)],
        }
    return block

def transform_markdown_for_mdbook(markdown_text):
    document = json.loads(pypandoc.convert_text(markdown_text, "json", "gfm"))
    cases = {}
    collect_testcases(document, cases)
    transformed_blocks = [convert_block_mdbook(block, cases) for block in document['blocks']]
    transformed_document = json.dumps({
        'blocks': transformed_blocks,
        'pandoc-api-version': document['pandoc-api-version'],
        'meta': document['meta'],
    })
    return pypandoc.convert_text(transformed_document, "gfm", "json")

def render_mdbook_summary_nodes(nodes, depth=1):
    lines = []
    indent = '  ' * depth

    for node in nodes:
        if node['kind'] == 'chapter':
            lines.append(f"# {node['title']}")
            lines.extend(render_mdbook_summary_nodes(node['children'], depth=1))
        elif node['kind'] == 'group':
            lines.append(f"{indent}- [{node['title']}]()")
            lines.extend(render_mdbook_summary_nodes(node['children'], depth=depth + 1))
        else:
            lines.append(f"{indent}- [{node['title']}]({node['file_name']}.md)")
            lines.extend(render_mdbook_summary_nodes(node['children'], depth=depth + 1))

    return lines

def copy_mdbook_sources(source_path, book_root, sidebar):
    src_dir = os.path.join(book_root, 'src')
    theme_dir = os.path.join(book_root, 'theme')
    theme_css_dir = os.path.join(theme_dir, 'css')
    os.makedirs(src_dir, exist_ok=True)
    os.makedirs(theme_css_dir, exist_ok=True)

    for name in os.listdir(source_path):
        if not name.endswith('.md') or name == '_Sidebar.md':
            continue
        source_file = os.path.join(source_path, name)
        with open(source_file, 'r') as f:
            transformed_markdown = transform_markdown_for_mdbook(f.read())
        with open(os.path.join(src_dir, name), 'w') as f:
            f.write(transformed_markdown)

    summary_lines = ['# Summary', '',
                        '']
    summary_lines.extend(render_mdbook_summary_nodes(sidebar['chapters']))
    summary_text = '\n'.join(summary_lines).rstrip() + '\n'

    with open(os.path.join(src_dir, 'SUMMARY.md'), 'w') as f:
        f.write(summary_text)

    book_toml = '\n'.join([
        '[book]',
        'title = "VerCors Tutorial"',
        'language = "en"',
        'src = "src"',
        '',
        '[output.html.playground]',
        'editable = true',
        'copyable = true',
        '',
    ])
    with open(os.path.join(book_root, 'book.toml'), 'w') as f:
        f.write(book_toml)

    head_override = '''
<link rel="stylesheet" href="/css/online.css">
<script src="/js/jquery.min.js"></script>
<script src="/js/vercorsonline.js"></script>
<script src="/js/init.js"></script>
<script>window.playground_line_numbers = true;</script>
<style>
html, body, #body-container, #page-wrapper, .page-wrapper, #content, .content {
    background: #e6e6e6 !important;
}

#content, .content, .content main {
    max-width: none !important;
    width: 90% !important;
    margin: auto
}

#sidebar {
    background: #e6e6e6 !important;
}
</style>
'''
    with open(os.path.join(theme_dir, 'head.hbs'), 'w') as f:
        f.write(head_override)

def output_mdbook(path, source_path, sidebar):
    if os.path.isdir(path):
        shutil.rmtree(path)
    os.makedirs(path, exist_ok=True)
    copy_mdbook_sources(source_path, path, sidebar)

if __name__ == "__main__":
    # TODO: Check if pypandoc is installed
    # TODO: Check if pandoc is installed, suggest installation methods

    parser = optparse.OptionParser()
    parser.add_option('-i', '--input', dest='source_path', help='directory where the wiki is stored', metavar='FILE')
    parser.add_option('-c', '--cases', dest='cases_path', help='write test cases extracted from the wiki to a folder')
    parser.add_option('--mdbook', dest='mdbook_path', help='write wiki to an mdBook project directory', metavar='FILE')


    options, args = parser.parse_args()

    if not any([options.cases_path, options.mdbook_path]):
        parser.error("No output type: please set one or more of the output paths. (try --help)")

    if options.source_path:
        source_path = options.source_path
    else:
        path = tempfile.mkdtemp()
        subprocess.run(["git", "clone", "https://github.com/utwente-fmt/vercors.wiki.git"], cwd=path)
        source_path = os.path.join(path, "vercors.wiki")
    
    if options.cases_path:
        print("Creating wiki test suite...")
        cases = {}

        for name in os.listdir(source_path):
            if not name.endswith('.md') or name == '_Sidebar.md':
                continue
            source_file = os.path.join(source_path, name)
            document = json.loads(pypandoc.convert_text(open(source_file).read(), "json", "gfm"))
            collect_testcases(document, cases)

        output_cases(options.cases_path, cases)
        print("done")
    if options.mdbook_path:
        print("Creating mdBook project...")
        sidebar = load_sidebar(source_path)
        output_mdbook(options.mdbook_path, source_path, sidebar)
