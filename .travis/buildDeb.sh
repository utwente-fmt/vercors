#!/bin/bash
set -euxo pipefail

.travis/travis_fold.sh buildDeb "Building VerCors .deb archive" "sbt debian:packageBin"

mkdir -p sync/${TRAVIS_BUILD_NUMBER}
cp target/*.deb sync/${TRAVIS_BUILD_NUMBER}/

source .travis/fileServer.sh
.travis/travis_fold.sh uploadBuildData "Uploading VerCors .deb archive" uploadBuildData

tree sync