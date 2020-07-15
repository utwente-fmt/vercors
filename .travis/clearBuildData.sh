#!/bin/bash
set -euxo pipefail

source .travis/fileServer.sh
.travis/travis_fold.sh clearBuildData "Clearing build data on server" clearBuildData
tree sync
