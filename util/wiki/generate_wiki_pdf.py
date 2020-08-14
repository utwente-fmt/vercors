import re
import subprocess
import tempfile
from urllib.parse import unquote
import pypandoc
import json
import os
import base64
import optparse


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
"""class Test {
$content$
}"""

    BLOCK = \
"""class Test {
    void test() {
$content$
    }
}"""

    def __init__(self, template_kind, verdict):
        self.template_kind = template_kind
        self.verdict = verdict
        self.content = None
        self.language = None

    def add_content(self, content):
        if self.content is not None:
            raise RuntimeError

        self.content = content

    def indent(self, amount, text):
        return '\n'.join("    " * amount + line for line in text.split("\n"))

    def render(self):
        if self.template_kind == 'test':
            return self.content
        elif self.template_kind == 'testMethod':
            return TemplateTestcase.METHOD.replace('$content$', self.indent(1, self.content))
        elif self.template_kind == 'testBlock':
            return TemplateTestcase.BLOCK.replace('$content$', self.indent(2, self.content))
        else:
            raise RuntimeError()


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

    # Get all chapter texts
    md_shifted_headers = []
    for name, file_name in chapters:
        with open(os.path.join(wiki_location, file_name + ".md"), "r") as f:
            md_shifted_headers.append(pypandoc.convert_text(
                f.read(), "gfm", "gfm", extra_args=["--base-header-level=2"]
            ))

    total_md = ""
    for (name, _), md in zip(chapters, md_shifted_headers):
        total_md += f"# {name}\n"
        total_md += md
        total_md += "\n"

    return json.loads(pypandoc.convert_text(total_md, "json", "gfm"))


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
                    cases[code_block_label] = TemplateTestcase(kind, args[0] if args else 'Pass')

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
        extra_args=["--toc"] if generate_toc else [])


if __name__ == "__main__":
    # TODO: Check if pypandoc is installed
    # TODO: Check if pandoc is installed, suggest installation methods

    parser = optparse.OptionParser()
    parser.add_option('-i', '--input', dest='source_path', help='directory where the wiki is stored', metavar='FILE')
    parser.add_option('-w', '--php', dest='php_path', help='write wiki to php file for the website', metavar='FILE')
    parser.add_option('-m', '--menu', dest='menu_path', help='extract a menu for the website', metavar='FILE')
    parser.add_option('-p', '--pdf', dest='pdf_path', help='write wiki to a latex-typeset pdf', metavar='FILE')

    options, args = parser.parse_args()

    if not any([options.php_path, options.menu_path, options.pdf_path]):
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

    collect_testcases(document, cases)

    blocks = document['blocks']

    if options.php_path:
        output_php(options.php_path, blocks, cases, pandoc_version)

    if options.menu_path:
        output_menu(options.menu_path, blocks, pandoc_version)

    if options.pdf_path:
        output_pdf(options.pdf_path, blocks, pandoc_version)
