# SplitVerify

SplitVerify is a tool to split the verification of a file, or a set of files, up into smaller chunks. SplitVerify will
then call an automated prover (currently VerCors) and attempt to prove all chunks. Chunks are cached between calls of
SplitVerify, thereby speeding up the verification process.

SplitVerify attempts to do the following:

- keep line numbers the same, so you can understand error messages as if they were pointing to the original (set of)
  file(s)
- cache calls irrespective of spacing and comments, so that changing small global details does not invalidate all caches
- err on the safe side: whenever you change something in the global scope (like adding a new function), you will
  invalidate all caches
- be language and prover independent (should work for many languages similar to pvl, jml, java)
- give some useful output as fast as possible

### Installation instructions

Install Stack, see https://docs.haskellstack.org/en/stable/install_and_upgrade/

Type 'stack install'

This should add 'SplitVerify' to a directory in your path, or give instructions on how to make sure that it will be.

### Installing dependencies

Install Vercors, ideally by cloning the git repository. See www.utwente.nl/vercors

Chances are that if you read this, you obtained SplitVerify as part of the VerCors repository

### Use

Create a settings file, which is easily done by calling:

```
SplitVerify --example > settings.sv
```

You can set up the path to vct, and to the vercors (pvl/java) files you'd like to see verified. If you're happy with
your settings file, run:

```
SplitVerify settings.sv
```

### Known issues

- If line numbers change, but the cache is not invalidated, then the line numbers in the error message no longer reflect
  the line numbers in the files
- The cache file generally just keeps growing, there is no garbage-collect
- Other issues and feature requests can be sent to ``sjcjoosten at gmail``
