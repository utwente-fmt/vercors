#!/bin/bash
set -euxo pipefail

source .travis/fileServer.sh
.travis/travis_fold.sh downloadBuildData "Downloading Vercors .deb file" downloadBuildData
tree sync

./.travis/travis_fold.sh build "Building VerCors" "./.travis/20-build.sh"
./.travis/travis_fold.sh fibonacci "Checking manual example" "./bin/vct --silicon $TRAVIS_BUILD_DIR/examples/manual/fibonacci.pvl"
./.travis/travis_fold.sh test "Executing tests" "./bin/vct --progress --test=$TRAVIS_BUILD_DIR/examples --travis-test-output --test-workers=1 --tool=silicon --exclude-suite=slow,medium,problem-fail,skip-travis --enable-test-coverage --coverage-output-file=sync/${TRAVIS_BUILD_NUMBER}/${TRAVIS_JOB_NUMBER}.xml"

tree sync
.travis/travis_fold.sh uploadBuildData "Uploading coverage .xml file" uploadBuildData
tree sync
