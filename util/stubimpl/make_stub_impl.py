import os
from pathlib import Path

"""
Create a file make.py with a definition like this:

OVERWRITE = False # whether or not the generated files should also overwrite files
MAKE = {
    "expr.literal": ["Apply", "Invocation"],
}
"""

def package_to_path(package):
    return package.replace('.', os.path.sep)

IMPL_PACKAGE = "vct.col.ast.temporaryimplpackage"
AST_PACKAGE = "vct.col.ast"

VERCORS_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
COL_DIR = os.path.join(VERCORS_DIR, "col/src/main/java")
IMPL_DIR = os.path.join(COL_DIR, package_to_path(IMPL_PACKAGE))

from make import MAKE, OVERWRITE

for package, nodes in MAKE.items():
    dir = os.path.join(IMPL_DIR, package_to_path(package))
    # make whatever directories don't exist in the chain
    Path(dir).mkdir(parents=True, exist_ok=True)

    for node in nodes:
        with open(os.path.join(dir, f"{node}Impl.scala"), "w" if OVERWRITE else "x") as f:
            f.write(f"package {IMPL_PACKAGE}.{package}\n")
            f.write("\n")
            f.write(f"import {AST_PACKAGE}.{node}\n")
            f.write("\n")
            f.write(f"trait {node}Impl[G] {'{'} this: {node}[G] =>\n")
            f.write("  \n")
            f.write("}\n")