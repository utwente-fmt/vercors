#!/usr/bin/env python3
import re
import subprocess
import tempfile
from urllib.parse import unquote
import pypandoc
import json
import os
import base64
import optparse
import time
import uuid

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

measurement_time = 0
def start_measuring_time():
    global measurement_time
    measurement_time = time.perf_counter()

def print_elapsed_time():
    global measurement_time
    print(f" Done ({time.perf_counter() - measurement_time:.2f}s)")

def collect_chapters(wiki_location):
    """
    convert a directory containing our github wiki repository to a list of json objects
    """
    with open(os.path.join(wiki_location, "_Sidebar.md"), "r") as f:
        contents = f.read()

    # Remove percentage/url encoding of ? and others
    contents = unquote(contents)

    # Matches sidebar entries
    chapter_re = re.compile("\[([A-Za-z /\?\&]+)\]\(https.*/([A-Za-z\-\?\&]+)\)")
    # Extract titles and last parts of links to chapters
    # Ignore "wiki" chapter, which is just a link to the wiki
    chapters = [chapter for chapter in chapter_re.findall(contents) if chapter[0] != "Home"]
    
    # Every md file is loaded and combined into one to ensure only one call is necessary to operate upon all the files. This saves time.
    # UUIDs are inserted at points where we later want to put a top-level title.
    print("Loading chapters...", end="")
    start_measuring_time()
    uuid_mappings = {}
    total_md = ""
    for (name, file_name) in chapters:
        chapter_code = uuid.uuid4()
        uuid_mappings[chapter_code] = f"# {name}"
        total_md += f"\n\n{chapter_code}\n\n"
        with open(os.path.join(wiki_location, file_name + ".md"), "r") as f:
            total_md += f.read()
        total_md += "\n"
    print_elapsed_time()

    # Every header is shifted by one to make sure only this script can insert top-level headings.
    print("Shifting headers...", end="")
    start_measuring_time()
    shifted_md = pypandoc.convert_text(total_md , "gfm", "gfm", extra_args=["--base-header-level=2"])
    print_elapsed_time()

    print("Replacing codes...", end="")
    start_measuring_time()
    shifted_titled_md = shifted_md
    for chapter_code in uuid_mappings:
        shifted_titled_md = shifted_titled_md.replace(str(chapter_code), uuid_mappings[chapter_code])
    print_elapsed_time()

    print("Converting to json...", end="")
    start_measuring_time()
    final_json = json.loads(pypandoc.convert_text(shifted_titled_md, "json", "gfm"))
    print_elapsed_time()
    return final_json

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
            cases[code_block_label].add_content(block['c'][1].strip())
            cases[code_block_label].language = block['c'][0][1][0]
            block['_case_label'] = code_block_label

        code_block_label = None

        # Headers are put into the breadcrumbs for template testcases
        if block['t'] == 'Header':
            # if the breadcrumbs are [Heading, Section, Subsection]
            # and we have a new section "Section 2"
            # the breadcrumbs should be [Heading, Section 2]
            breadcrumbs = breadcrumbs[:block['c'][0]]
            breadcrumbs += ['?'] * (block['c'][0] - len(breadcrumbs))
            breadcrumbs[block['c'][0] - 1] = block['c'][1][0]
            testcase_number = 1

        # Raw blocks that are comments starting with something we recognize are processed
        if block['t'] == 'RawBlock' and block['c'][0] == 'html':
            content = block['c'][1].strip()
            if content.startswith('<!--') and content.endswith('-->'):
                lines = [line.strip() for line in content[4:-3].strip().split('\n')]
                kind, *args = lines[0].split(' ')

                # Template label
                if kind in {'testBlock', 'testMethod', 'test'}:
                    code_block_label = '-'.join(breadcrumbs) + '-' + str(testcase_number)
                    testcase_number += 1
                    cases[code_block_label] = TemplateTestcase(code_block_label, kind, args[0] if args else 'Pass')

                # Snippet
                if kind == 'standaloneSnip':
                    label = breadcrumbs[0] + '-' + args[0]

                    if label not in cases:
                        cases[label] = SnippetTestcase()

                    cases[label].add_content('\n'.join(lines[1:]) + '\n')

                # Snippet label for code block
                if kind == 'codeSnip':
                    code_block_label = breadcrumbs[0] + '-' + args[0]

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


def get_html(elements):
    result = ""

    for element in elements:
        if element['t'] == 'Str':
            result += element['c']
        elif element['t'] == 'Space':
            result += ' '
        else:
            assert False, element['t']

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
            if not "//:: tool" in content:
                raise CaseWithoutTool(f'Case "{case_name}" is missing a tool directive for the test suite')
            with open(p, "w") as f:
                f.write(case.render())
            ok += 1
        except UnknownLanguageError:
            print(f"Unknown language {case.language} in case {case_name}")
            not_ok += 1

    print(f"Extracted {ok} cases successfully. {not_ok} cases failed.")

    if not_ok > 0:
        raise CasesExtractionFailed

if __name__ == "__main__":
    # TODO: Check if pypandoc is installed
    # TODO: Check if pandoc is installed, suggest installation methods

    parser = optparse.OptionParser()
    parser.add_option('-i', '--input', dest='source_path', help='directory where the wiki is stored', metavar='FILE')
    parser.add_option('-w', '--php', dest='php_path', help='write wiki to php file for the website', metavar='FILE')
    parser.add_option('-m', '--menu', dest='menu_path', help='extract a menu for the website', metavar='FILE')
    parser.add_option('-p', '--pdf', dest='pdf_path', help='write wiki to a latex-typeset pdf', metavar='FILE')
    parser.add_option('--html', dest='html_path', help='write wiki to an html file', metavar='FILE')
    parser.add_option('-c', '--cases', dest='cases_path', help='write test cases extracted from the wiki to a folder')


    options, args = parser.parse_args()

    if not any([options.php_path, options.menu_path, options.pdf_path, options.html_path]) and not options.cases_path:
        parser.error("No output type: please set one or more of the output paths. (try --help)")

    if options.source_path:
        source_path = options.source_path
    else:
        path = tempfile.mkdtemp()
        subprocess.run(["git", "clone", "https://github.com/utwente-fmt/vercors.wiki.git"], cwd=path)
        source_path = os.path.join(path, "vercors.wiki")

    document = collect_chapters(source_path)
    pandoc_version = document['pandoc-api-version']
    cases = {}

    print("Collecting test cases...")
    collect_testcases(document, cases)

    blocks = document['blocks']

    if options.php_path:
        print("Creating PHP...")
        output_php(options.php_path, blocks, cases, pandoc_version)

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
