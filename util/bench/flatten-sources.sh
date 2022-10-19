#!/bin/bash

SCRIPT=$(realpath $0)
BENCH=$(dirname $SCRIPT)
UTIL=$(dirname $BENCH)
ROOT=$(dirname $UTIL)

echo "Generating sources and extracting paths..."
OUTPUT=$(cd $ROOT; sbt --error "col / generateHelpersTask; parsers / antlrTask; benchPrintExternalDeps; benchPrintSources")
EXTERNAL_DEPS=$(head -n 1 <<< "$OUTPUT")
SOURCES=$(tail -n +2 <<< "$OUTPUT")

echo "Removing previous generated project..."
cd $BENCH
rm -rf project
mkdir -p project
cd project

cp $BENCH/build.sbt.template build.sbt
echo "Compile / unmanagedClasspath ++= $EXTERNAL_DEPS" >> build.sbt

mkdir -p src/main
cd src/main

for src in $SOURCES; do
  echo "Copying $src..."
  cp -r $src/main/*a .
done
