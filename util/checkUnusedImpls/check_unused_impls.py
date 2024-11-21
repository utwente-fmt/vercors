from pathlib import Path

src = Path("src")

impls = []
for (dirpath, dirnames, filenames) in src.walk():
    for filename in filenames:
        path = dirpath / filename
        if path.name.endswith("Impl.scala"):
            impls += [path.stem]

with open("src/col/vct/col/ast/Node.scala", "r") as f:
    node_src = f.read()

unused = []
for impl_type in impls:
    if not (impl_type in node_src):
        print(impl_type)   
        unused += [impl_type]

if unused != []:
    import sys
    sys.exit(1)
