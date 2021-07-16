VerCors Verification Toolset
=======

This is VerCors, a toolset for the verification of concurrent and parallel programs. VerCors aims to verify many
different concurrency constructs, including: heterogeneous concurrency (Java and C), GPU kernels using barriers and
atomics (OpenCL), and compiler directives as used in deterministic parallelism (OpenMP). VerCors is able to prove
data-race freedom, memory safety, and functional correctness of (concurrent) programs written in Java, C, OpenCL,
OpenMP, and PVL (Prototypal Verification Language, a procedural toy language for prototyping new verification features).
Moreover, VerCors is designed to be language-independent, which makes adding new front-end languages a straightforward
engineering effort.

## Dependencies

VerCors requires a java runtime environment (version 8 or later), as well as clang if you want support for C.

### Linux

Currently we support debian-based systems; let us know if you need something else! Install the dependencies:

```shell script
sudo apt install clang openjdk-8-jre 
```

Obtain the latest deb release of VerCors [here](https://github.com/utwente-fmt/vercors/releases/latest), and install it
by running:

```shell script
sudo dpkg -i Vercors_x.y.z_all.deb
```

### Mac

You can for example obtain the dependencies through homebrew:

```shell script
brew cask install java
```

This should install the latest release of OpenJDK. Clang should already be installed through XCode.

Obtain the latest zip release of VerCors [here](https://github.com/utwente-fmt/vercors/releases/latest) and unzip it.
You can find the run script for VerCors in the `bin` subdirectory.

### Windows

You can obtain a java runtime environment e.g. [here](https://jdk.java.net). Make sure that the environment
variable `JAVA_HOME` points to wherever you unpack the JDK. clang can be obtained as part of the llvm
toolchain [here](https://clang.llvm.org/). Make sure that `clang` is added to the path.

Next, download the latest zip release of VerCors [here](https://github.com/utwente-fmt/vercors/releases/latest) and
unzip it. You can find the batch script for VerCors in the `bin` subdirectory.

## Usage

VerCors verifies programs that are annotated with JML-style specifications (the underlying theory uses separation logic
with permission accounting). Details on the specification language can be found on the
VerCors [Wiki pages](https://github.com/utwente-fmt/vercors/wiki). Furthermore, a large collection of example programs
can be found (and verified) in the `./examples` directory.

The VerCors toolset can be used by running `vercors --silicon <filepath>`, with `<filepath>` the path of the (Java, C,
or PVL) file to verify.