#!/bin/bash

./.travis/travis_fold.sh tar "Patching cache utilities" "./.travis/10-tar.sh"
./.travis/travis_fold.sh build "Building VerCors" "./.travis/20-build.sh"
./.travis/travis_fold.sh fibonacci "Checking manual example" "vct --silicon $TRAVIS_BUILD_DIR/examples/manual/fibonacci.pvl"
./.travis/travis_fold.sh test "Executing tests" "vct --tool=silicon --exclude-suite=slow,medium,problem-fail,skip-travis --test=$TRAVIS_BUILD_DIR/examples --progress"