VerCors Verification Toolset
=======

This is VerCors, a toolset for the verification of concurrent and parallel programs. VerCors aims to verify many different concurrency constructs, including: heterogeneous concurrency (Java and C), GPU kernels using barriers and atomics (OpenCL), and compiler directives as used in deterministic parallelism (OpenMP). VerCors is able to prove data-race freedom, memory safety, and functional correctness of (concurrent) programs written in Java, C, OpenCL, OpenMP, and PVL (Prototypal Verification Language, a procedural toy language for prototyping new verification features). Moreover, VerCors is designed to be language-independent, which makes adding new front-end languages a straightforward engineering effort.

