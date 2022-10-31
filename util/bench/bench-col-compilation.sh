#!/usr/bin/env bash

SCRIPT=$(realpath $0)
BENCH=$(dirname $SCRIPT)
UTIL=$(dirname $BENCH)
ROOT=$(dirname $UTIL)

rm -rf $ROOT/col/target
(cd $ROOT; time sbt "col / compile")