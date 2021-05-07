#!/usr/bin/env bash

CMD=""

# Detect vercors, path might not be consistent
if command -v vct-dev &> /dev/null
then
    CMD="vct-dev"
elif command -v vct &> /dev/null
then
    CMD="vct"
elif command -v vercors &> /dev/null
then
    CMD="vercors"
else
    echo "VerCors was not found, please put \"vct\" or \"vercors\" binary in PATH"
    exit 1
fi

echo "Using command \"$CMD\""

$CMD --silicon --test=. --progress
