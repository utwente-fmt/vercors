#!/bin/bash
set -euo pipefail

source .travis/fileServer.sh
.travis/travis_fold.sh downloadBuildData "Downloading Vercors .deb file" downloadBuildData
tree sync

# Make swap file to decrease the change that the build fails because of OOM
sudo dd if=/dev/zero of=/swapfile_4G.img bs=1024 count=4M
sudo mkswap /swapfile_4G.img
sudo swapon -p 20 /swapfile_4G.img

./.travis/travis_fold.sh install "Installing VerCors" "sudo dpkg -i sync/${TRAVIS_BUILD_NUMBER}/Vercors*.deb"
./.travis/travis_fold.sh fibonacci "Checking manual example" "vercors --silicon $TRAVIS_BUILD_DIR/examples/manual/fibonacci.pvl"
./.travis/travis_fold.sh test "Executing tests" "vercors --progress --test=$TRAVIS_BUILD_DIR/examples --travis-test-output --test-workers=2 --tool=silicon --exclude-suite=slow,medium,problem-fail,skip-travis --enable-test-coverage --coverage-output-file=sync/${TRAVIS_BUILD_NUMBER}/${TRAVIS_JOB_NUMBER}.xml"

tree sync
.travis/travis_fold.sh uploadBuildData "Uploading coverage .xml file" uploadBuildData
tree sync
