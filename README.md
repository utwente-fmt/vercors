VerCors Verification Toolset ![GitHub Workflow Status (master)](https://img.shields.io/github/workflow/status/utwente-fmt/vercors/Vercors%20build%20and%20test%20workflow/master?label=master&style=flat-square) ![GitHub Workflow Status (dev)](https://img.shields.io/github/workflow/status/utwente-fmt/vercors/Vercors%20build%20and%20test%20workflow/dev?label=dev&style=flat-square)
=======

This repository hosts VerCors, a toolset for the verification of concurrent and parallel programs. VerCors aims to verify many different concurrency constructs, including: heterogeneous concurrency (Java and C), GPU kernels using barriers and atomics (OpenCL), and compiler directives as used in deterministic parallelism (OpenMP). VerCors is able to prove data-race freedom, memory safety, and functional correctness of (concurrent) programs written in Java, C, OpenCL, OpenMP, and PVL (Prototypal Verification Language, a procedural toy language for prototyping new verification features). Moreover, VerCors is designed to be language-independent, which makes adding new front-end languages a straightforward engineering effort.

A list of verified examples and case studies is maintained online and can be found [here](https://utwente.nl/vercors). This webpage also contains an online interface for VerCors and allows you to try VerCors online.

### The tutorial can be found on the [VerCors Wiki](https://github.com/utwente-fmt/vercors/wiki)!

# Installation
You can install VerCors by either using a release (recommended for beginners), or by building VerCors from its source code.

## Using a Release
VerCors requires a **java** runtime environment (version 11 or later), as well as **clang** if you want support for C.

### Debian Linux
Currently we support debian-based systems; let us know if you need something else! Install the dependencies:

```shell script
sudo apt install clang openjdk-11-jre
```

Obtain the latest `deb` release of VerCors [here](https://github.com/utwente-fmt/vercors/releases/latest), and install it by running:

```shell script
sudo dpkg -i Vercors_x.y.z_all.deb
```

VerCors should now be on the path:

```shell script
$ vercors --silicon /usr/share/vercors/examples/manual/array.pvl
Success!
The final verdict is Pass
```

### Mac
You can for example obtain the dependencies through homebrew:

```shell script
brew install java
```

This should install the latest release of OpenJDK. Clang should already be installed through XCode.

Obtain the latest `tgz` release of VerCors [here](https://github.com/utwente-fmt/vercors/releases/latest) and unpack it. You can find the run script for VerCors in the `bin` subdirectory:

```shell script
$ ./bin/vercors --silicon examples/manual/array.pvl
Success!
The final verdict is Pass
```

**NB**: you may need to allow the dependencies of VerCors to run manually. If you get an error that z3 and/or boogie is not from a trusted developer, execute them once manually from Finder and add an exception. They are located at:

* `res/deps/z3/4.8.6/Darwin/x86_64/bin/z3`
* `res/deps/boogie/1.0.0.0-carbon/Darwin/Boogie`

### Windows
You can obtain a java runtime environment e.g. [here](https://jdk.java.net). Make sure that the environment variable `JAVA_HOME` points to wherever you unpack the JDK. clang can be obtained as part of the llvm toolchain [here](https://clang.llvm.org/). Make sure that `clang` is added to the path.

Next, download the latest zip release of VerCors [here](https://github.com/utwente-fmt/vercors/releases/latest) and unzip it. You can find the batch script for VerCors in the `bin` subdirectory:

```shell script
> bin\vercors.bat --silicon examples\manual\array.pvl
Success!
The final verdict is Pass
```

## Building from source code
When building VerCors, you additionally need these dependencies:

- A Java _Development_ Kit, version 11 or greater, either OpenJDK or Oracle.
- Git (on Windows you need Git Bash, see <https://git-scm.com/downloads>)
- Scala SBT, version 1.3.0 or greater (see <http://www.scala-sbt.org> for instructions)

1. Clone the VerCors repository using `git clone https://github.com/utwente-fmt/vercors.git` and move into the cloned directory, `cd vercors`.
2. Run `sbt compile` to compile VerCors.
3. Test whether the build was successful by running `./bin/vct --test=examples/manual --tool=silicon --lang=pvl,java --progress`.

The last command tests the VerCors installation by verifying a large collection of examples (from the `./examples` directory). This command should eventually report that `all ? tests passed`. There are also intstructions for importing VerCors into either eclipse or IntelliJ IDEA [here](https://github.com/utwente-fmt/vercors/wiki).


# Running VerCors
VerCors verifies programs that are annotated with JML-style specifications (the underlying theory uses separation logic with permission accounting). Details on the specification language can be found on the VerCors [Wiki pages](https://github.com/utwente-fmt/vercors/wiki). Furthermore, a large collection of example programs can be found (and verified) in the `./examples` directory.

The VerCors toolset can be used by running `vercors --silicon <filepath>`, with `<filepath>` the path of the (Java, C, or PVL) file to verify.


## Contact
- For questions and support, email to <vercors@lists.utwente.nl>.
- For bug reports and feature requests, visit <https://github.com/utwente-fmt/vercors/issues>.

## Related papers
A complete list of papers on the VerCors project is given [here](https://vercors.ewi.utwente.nl/publications).

## License
Copyright (c) 2008 - 2022 Formal Methods and Tools, University of Twente
All rights reserved.

The license to VerCors is a mozilla open source license as described in LICENSE.TXT in the root of this project. It is a free to use, share-alike license. Should this license be too restrictive for your purpose, please let us know by creating an issue in our bug tracker. Direct contributors (people who send us pull-requests or edit this repository directly) are expected to agree with any license that the University of Twente might decide. If you do not agree with future license changes, please instead fork this repository as allowed under the conditions of LICENSE.TXT.
