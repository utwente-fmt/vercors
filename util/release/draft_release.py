#!/usr/bin/env python3
import subprocess
import sys
import os.path

def confirm(command, cwd):
	# This may not be exactly right with spaces and such, but it's not how we run it, so it's okay.
	command_text = " ".join(command)

	while 1:
		inp = input(f"Run `{command_text}`? [Y/n] ")
		if inp in {"Y", "y", ""}:
			result = subprocess.run(command, capture_output=True, cwd=cwd)
			if result.returncode != 0:
				print(f"Exiting, since the exit code is {result.returncode}.")
				print(result.stdout.decode('utf-8'), end="")
				print(result.stderr.decode('utf-8'), end="")
				sys.exit(1)
			return result.stdout.decode('utf-8')
		elif inp in {"N", "n"}:
			print("Aborting.")
			sys.exit(1)


print("Obtain the root of the VerCors repository")
root = confirm(["git", "rev-parse", "--show-toplevel"], None).strip()
print("Obtain the intended version of VerCors")
version = confirm(["sbt", "version"], root)
lines = version.split("\n")
index = lines.index("[info] version")
version = lines[index+1].replace("[info]", "").strip()
print(f"Building a release for version {version}")
print("Generate packages")
confirm(["sbt", "universal:packageBin", "universal:packageZipTarball", "debian:packageBin"], root)

deb_location = os.path.join(root, "target", f"Vercors_{version}_all.deb")
tar_location = os.path.join(root, "target", "universal", f"vercors-{version}.tgz")
zip_location = os.path.join(root, "target", "universal", f"vercors-{version}.zip")

confirm(["mv", "-n", deb_location, os.path.join(root, f"vercors-{version}-debian.deb")], root)
confirm(["mv", "-n", tar_location, os.path.join(root, f"vercors-{version}-mac.tar.gz")], root)
confirm(["mv", "-n", zip_location, os.path.join(root, f"vercors-{version}-windows.zip")], root)

# confirm(["git", "tag", f"v{version}"], root)
# confirm(["git", "push", "origin", f"v{version}"], root)

# confirm(["xdg-open", f"https://github.com/utwente-fmt/vercors/releases/new?tag=v{version}&title=VerCors%20{version}"], root)

print("All done!")
