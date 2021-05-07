#!/usr/bin/env bash

JSTAR_PATH=~/PhD/drafts/FMICS-2021-exceptions/artefacts/jstar

for filename in $(find -name '*.java'); do
    echo "----------------------------------------"
    echo "$filename: testing"
    output=$($JSTAR_PATH/jstar.sh $filename 2>&1)
    if [ "$?" -eq "1" ]
    then
        echo "$filename: failure. Output:"
        echo "$output\n\n"
    else
        echo "$filename: success"
    fi
    (cd $(dirname $filename); $JSTAR_PATH/cleanJstar.sh)
done
