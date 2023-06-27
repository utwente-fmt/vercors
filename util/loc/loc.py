import os
from collections import defaultdict

CATEGORY = [
	("vercors/tmp", "temp"),
	("vercors/out", "temp"),
	("vercors/examples/private", "temp"),
	("vercors/.idea", "temp"),
	("vercors/.bsp", "temp"),

	("vercors/README.md", "misc"),
	("vercors/LICENSE.txt", "misc"),
	("vercors/mill", "misc"),
	("vercors/mill.bat", "misc"),
	("vercors/.git", "misc"),
	("vercors/.github", "misc"),
	("vercors/bin", "misc"),
	("vercors/util", "misc"),
	("vercors/examples", "misc"),
	("vercors/res/universal/README.md", "misc"),

	("vercors/res/hre", "config"),
	("vercors/res/universal/res/systemc", "config"),

	("vercors/project", "build"),
	("vercors/build.sc", "build"),
	("vercors/.mill-jvm-opts", "build"),
	("vercors/.mill-version", "build"),

	("vercors/test", "test"),

	("vercors/res/universal/deps", "res"),
	("vercors/lib", "res"),

	("vercors/res/universal/res/jdk", "adt"),
	("vercors/res/universal/res/c", "adt"),
	("vercors/res/universal/res/include", "adt"),
	("vercors/res/universal/res/adt", "adt"),
	("vercors/res/universal/res/simplify", "adt"),

	("vercors/src/main/vct/options", "ui"),
	("vercors/src/main/vct/main/Main.scala", "ui"),
	("vercors/src/main", "base"),
	("vercors/src/col", "base"),
	("vercors/src/parsers/antrl4", "grammar"),
	("vercors/src/parsers", "base"),
	("vercors/src/rewrite", "base"),
	("vercors/src/viper", "base"),
	("vercors/src/hre/hre/progress", "ui"),
	("vercors/src/hre/hre/perf", "ui"),
	("vercors/src/hre", "base"),
]

def get_category(f):
	for start, category in CATEGORY:
		if f.startswith(start):
			return category

	return None

def verify_accounting():
	for base, dirs, files in os.walk("vercors"):
		if get_category(base) is not None:
			continue

		for file in files:
			if get_category(base + "/" + file) is None:
				raise ValueError(f"File {file} in directory {base} is not accounted for")

def account_by_category():
	res = defaultdict(lambda: defaultdict(list))

	for base, dirs, files in os.walk("vercors"):
		for file in files:
			path = base + "/" + file
			cat = get_category(path)
			
			try:
				ws = 0
				non_ws = 0
				with open(path, "r") as f:
					for line in f.readlines():
						if line.strip():
							non_ws += 1
						else:
							ws += 1
				
				res[cat]["empty_lines"].append((ws, file))
				res[cat]["nonempty_lines"].append((non_ws, file))
			except UnicodeDecodeError:
				res[cat]["nontext_bytes"].append((os.path.getsize(path), file))

	return res

if __name__ == "__main__":
	verify_accounting()
	res = account_by_category()

	for cat, kinds in res.items():
		print()
		print(cat)
		for kind, amounts in kinds.items():
			worst = ", ".join(path for _, path in sorted(amounts)[-3:])
			acc = 0
			for amount, _ in amounts:
				acc += amount

			print(f"{kind}: {acc} (e.g. {worst})")
