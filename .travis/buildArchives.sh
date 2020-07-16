#!/bin/bash
set -euo pipefail

.travis/travis_fold.sh buildDeb "Building VerCors .deb, .txz archive" "sbt debian:packageBin universal:packageXzTarball"

mkdir -p sync/${TRAVIS_BUILD_NUMBER}
cp target/*.deb sync/${TRAVIS_BUILD_NUMBER}/
cp target/*.txz sync/${TRAVIS_BUILD_NUMBER}/

source .travis/fileServer.sh
.travis/travis_fold.sh uploadBuildData "Uploading VerCors archives" uploadBuildData

tree sync