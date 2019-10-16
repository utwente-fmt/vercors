#!/bin/bash
set -e

printf "\033[0Ktravis_fold:start:$1\r"
printf "\033[0K$2\n"

eval $3

printf "travis_fold:end:$1\r"
