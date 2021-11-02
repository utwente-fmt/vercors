# VerCors wiki script

## Dependencies

- At least python 3.6
- `pandoc`: `sudo apt install pandoc`
- `pypandoc`: `pip install -r requirements.txt`
  If you prefer, a virtualenv can also be created. The only real requirement is is that the pypandoc library is available.

## General usage

Run the script with `--help`.

## PDF generation

Xelatex is needed because the wiki uses unicode characters here and there. This is usually included in most TeX distributions (i.e. TeXLive-full on ubuntu), so you probably already have it by default. If not, you might have to install it separately.
