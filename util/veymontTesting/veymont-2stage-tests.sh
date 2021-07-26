#!/usr/bin/env bash

# This is a temporary veymont testing script.
#
# It finds all cases in examples/veymont-global-programs, runs them through veymont, and runs the output
# through vercors.
#
# It ignores all other features of the vercors testing framework (e.g. suites, options). If this script gets any more
# features it should, at least, be re-implemented in scala, but ideally it would be integrated in the test suite.

vercorsPath=$(which vct-dev || which vct || which vercors)
if [ -z $vercorsPath ]; then
  echo "Could not detect vct-dev, vct, or vercors executable path"
  exit 1
fi
echo "VerCors path: $vercorsPath"

# Cd to the directory of the shell script
cd "$(dirname "$0")"

cd ../../examples/veymont-global-programs

# Collect all cases. The cut part of the invocation selects the third element after splitting on spaces.
allCases=$(grep -r cases | cut -d " " -f 3 | sort -u)

echo "##### Detected cases for 2-stage tests: #####"
echo "$allCases"

failingReport="##### Failing 2-stage cases #####"
# This is the final return code to be returned
# In unix-style, 0 is success, anything else is failure
# So if at some point it is set to something non-zero, it means something went wrong
totalReturnCode=0

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
    failingReport="$failingReport\n- $kees"
    totalReturnCode=1
    continue
  fi

  vercorsCmd="$vercorsPath --silicon ${kees}_output.pvl"
  echo "-- Running VerCors: $vercorsCmd"
  output=$(eval $vercorsCmd)
  echo "$output"
  if [[ $output != *"Pass"* ]]; then
    echo "VerCors did not succeed in analyzing the VeyMont output for case $kees"
    failingReport="$failingReport\n- $kees"
    totalReturnCode=1
  fi
done

printf "$failingReport\n"
exit $totalReturnCode
