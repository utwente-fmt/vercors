#!/usr/bin/env bash

# This is a temporary veymont testing script. It finds all cases in examples/veymont-global-programs, runs them through
# veymont, and then runs the output through vercors. It ignores all other features of the vercors testing framework
# (e.g. suites, options). If this script gets any more features it should, at least, be re-implemented in scala,
# but ideally it would be integrated in the test suite.

vercorsPath=$(which vct-dev || which vct || which vercors)
if [ -z $vercorsPath ]; then
  echo "Could not detect vct-dev, vct, or vercors executable path"
  exit 1
fi
echo "VerCors path: $vercorsPath"

# Move to the directory of the shell script
cd "$(dirname "$0")"
# Move to veymont global tests
cd ../../examples/veymont-global-programs

# Collect all cases. For now we only collect the first 4 cases of each file, as there are no tests with more than 2 cases anyway
cases1=$(grep -r cases | cut -d " " -f 3)
#                |           |      - Print the third element after splitting
#                |           - Split at space
#                - search for "cases"
cases2=$(grep -r cases | cut -d " " -f 4)
cases3=$(grep -r cases | cut -d " " -f 5)
cases4=$(grep -r cases | cut -d " " -f 6)
# The awk invocation drops all empty lines from stdin and prints the leftover lines
# Also, use printf because echo ignores \n newlines
allCases=$(printf "$cases1\n$cases2\n$cases3\n$cases4\n" | awk NF | sort -u)

echo "##### Detected cases: #####"
echo "$allCases"

for kees in $allCases
do
  echo "##### Case: $kees #####"
  # Find all files that have the case $kees
  # Replacing newlines with spaces to make sure all the filenames are on one line
  files=$(grep -r $kees -l | tr "\n" " ")

  veymontCmd="$vercorsPath --veymont ${kees}_output.pvl $files"
  echo "-- Running VeyMont: $veymontCmd"
  output=$(eval $veymontCmd)
  echo "$output"
  if [[ $output != *"Pass"* ]]; then
    echo "VeyMont did not succeed in analyzing case $kees, files: $files"
    exit 1
  fi

  vercorsCmd="$vercorsPath --silicon ${kees}_output.pvl"
  echo "-- Running VerCors: $vercorsCmd"
  output=$(eval $vercorsCmd)
  echo "$output"
  if [[ $output != *"Pass"* ]]; then
    echo "VerCors did not succeed in analyzing the VeyMont output for case $kees"
    exit 1
  fi
done