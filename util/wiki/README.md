# VerCors wiki script

## Dependencies

- At least python 3.6
- `pandoc`: `sudo apt install pandoc`
- `pypandoc`: `pip install -r requirements.txt`
  If you prefer, a virtualenv can also be created. The only real requirement is is that the pypandoc library is available.

Not abiding by these requirements will result in a fiery crash when running the script.

## General usage

Run the script with `--help` for general usage info.

## PDF generation

Xelatex is needed because the wiki uses unicode characters here and there. This is usually included in most TeX distributions (i.e. TeXLive-full on ubuntu), so you probably already have it if you have a complete latex distribution. If not, you might have to install it separately.

## Debugging example snippets in the wiki

Copy-pasting code from the wiki into a local file, debugging it with VerCors, and copying the corrected file back to the wiki, is a tedious process. Here we describe a way to debug a specific snippet in the wiki in a less tedious way. Specifically, you can directly add your changes to a local copy of the wiki, and then with one command test if the snippet passes or fails, without copying code back and forth from the local copy of the wiki.

An important step to make this fast is to clone the wiki in the same folder as the wiki script:

```bash
~/vercors/util/wiki$ git clone git@github.com:utwente-fmt/vercors.wiki.git
```

With the vercors wiki cloned locally, the only bottleneck in extracting all the snippets is the script itself. Since extracting snippets with the script takes around two seconds, extracting a snippet and then running vercors on it is almost as interactive as just running VerCors on some file.

To test if a wiki snippet passes or fails, the following command can be used:

```bash
~/vercors/util/wiki$ rm -rf cases && ./generate_wiki_pdf.py -i vercors.wiki -c cases && vercors --silicon cases/specification-syntax-method-contracts-1.java
```

This command will:

1. Remove the `cases` directory, in case it exists because of previous runs.
2. Extract all wiki snippets to be found in the folder `vercors.wiki` into the folder `cases`, creating the folder if it does not exist.
3. Run `vercors` on one of the extracted snippets. If you want to verify a different snippet, be sure to change this filename.

For this command to work, the vercors wiki must be cloned in the `util/wiki` folder, and there should be a globally available executable named `vercors`. Of course if your setup is different, this must be adjusted in the command.

Next, we explain the naming scheme of snippets.

## Snippet naming convention used by the script

Each snippet in the wiki gets written to a file with a name similar to where it is located in the wiki. For example, the first snippet of Java code located in subsection "Method Contracts" of chapter "Specification Syntax" will be called "specification-syntax-method-contracts-1.java". If there is more than one snippet in a section, the number at the end is increased. If snippets are nested more deeply, the subsub- and subsubsubsection titles will be included in the snippet filename. If a title or subsection is missing, the script will insert a `?`.

Standalone snippets must choose their own snippet name. In that case, the filename is equal to "chapter-name-snippet-name". In other words, for standalone snippets, the subsections are ignored, and wiki authors should take care that the case names of standalone snippets do not unintentionally overlap.
