import re
import os
import sys
import tempfile
import shutil

PATTERN = r"([0-9]+[ \s]*)/([0-9]+[ \s]*)"

def save_changes():
    paths = []
    for path, names, files in os.walk('examples'):
        for file in files:
            paths.append(os.path.join(path, file))

    with open("changes.txt", "w") as changes:
        for file in paths:
            header = False
            input = open(file, "r")
            for i, line in enumerate(input):
                if re.search(PATTERN, line):
                    if not header:
                        changes.write(file + '\n')
                        header = True
                    changes.write("{}: {}\n".format(i+1, line.strip()))

            if header:
                changes.write("\n")

    print("The proposed changes have been writting to changes.txt. Consult them, change as necessary and then rerun with --commit.")


def process(file, lines):
    if file is not None:
        with open(file, "r") as input, tempfile.NamedTemporaryFile('w', delete=False) as output:
            for i, line in enumerate(input):
                if i+1 in lines:
                    output.write(re.sub(PATTERN, r"\1f/\2", line))
                else:
                    output.write(line)

            name = output.name

        shutil.move(name, file)


def commit():
    with open("changes.txt", "r") as changes:
        file = None
        lines = set()

        for line in changes:
            if not line.strip(): continue
            try:
                lines.add(int(line.strip().split(":")[0]))
            except ValueError:
                process(file, lines)
                file = line.strip()
                lines = set()

        process(file, lines)

if __name__ == "__main__":
    if len(sys.argv) == 2 and sys.argv[1] == "--commit":
        commit()
    else:
        save_changes()
