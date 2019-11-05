VerCors Verification Toolset ![master status](https://img.shields.io/travis/utwente-fmt/vercors/master?label=master) ![dev status](https://img.shields.io/travis/utwente-fmt/vercors/dev?label=dev)
=======

This repository hosts VerCors, a toolset for the verification of concurrent and parallel programs. VerCors aims to verify many different concurrency constructs, including: heterogeneous concurrency (Java and C), GPU kernels using barriers and atomics (OpenCL), and compiler directives as used in deterministic parallelism (OpenMP). VerCors is able to prove data-race freedom, memory safety, and functional correctness of (concurrent) programs written in Java, C, OpenCL, OpenMP, and PVL (Prototypal Verification Language, a procedural toy language for prototyping new verification features). Moreover, VerCors is designed to be language-independent, which makes adding new front-end languages a straightforward engineering effort.

A list of verified examples and case studies is maintained online and can be found [here](http://ctit-vm2.ewi.utwente.nl). This webpage also contains an online interface for VerCors and allows to try VerCors online.

The latest release of VerCors can be downloaded [here](https://github.com/utwente-fmt/vercors/releases/latest).

Installation Instructions
-------------

The VerCors toolset can be installed and used on MacOS X, Linux, and Windows (via Cygwin). When using Windows make sure that the environment variable `JAVA_HOME` is configured. A basic installation requires:

- A Java Development Kit, version 8 or greater, either OpenJDK or Oracle.
- Git (on Windows you need Git Bash, see <https://git-scm.com/downloads>)
- Mercurial (See <https://www.mercurial-scm.org/downloads>)
- Scala SBT, version 1.3.0 or greater (see <http://www.scala-sbt.org> for instructions)
- If you want support for C, you need clang (see <https://clang.llvm.org/>)

If you have these requirements, proceed to ['Building'](#building).

### Building

For a basic build of VerCors the following steps should be taken:

1. Clone the VerCors repository using `git clone https://github.com/utwente-fmt/vercors.git` and move into the cloned directory, `cd vercors`.
2. Run `sbt compile` to compile VerCors.
3. Test whether the build was successful by running `./bin/vct --test=examples/manual --tool=silicon --lang=pvl,java --progress`.

The last command tests the VerCors installation by verifying a large collection of examples (from the `./examples` directory). This command should eventually report that `all ? tests passed`.

Usage instructions
-------------

VerCors verifies programs that are annotated with JML-style specifications (the underlying theory uses separation logic with permission accounting). Details on the specification language can be found on the VerCors [Wiki pages](https://github.com/utwente-fmt/vercors/wiki). Furthermore, a large collection of example programs can be found (and verified) in the `./examples` directory.

The VerCors toolset can be used from the main directory by running `./bin/vct --silicon <filepath>`, with `<filepath>` the path of the (Java, C, or PVL) file to verify.

## Contact

- For questions and support, email to <vercors@lists.utwente.nl>.
- For bug reports and feature requests, visit <https://github.com/utwente-fmt/vercors/issues>.

## Related papers

A complete list of papers on the VerCors project is given [here](http://eprints.eemcs.utwente.nl/view/project/VerCors.html).

## License

Copyright (c) 2008 - 2018 Formal Methods and Tools, University of Twente
All rights reserved.

The license to VerCors is a mozilla open source license as described in LICENSE.TXT in the root of this project. It is a free to use, share-alike license. Should this license be too restrictive for your purpose, please let us know by creating an issue in our bug tracker. Direct contributors (people who send us pull-requests or edit this repository directly) are expected to agree with any license that the University of Twente might decide. If you do not agree with future license changes, please instead fork this repository as allowed under the conditions of LICENSE.TXT.
