#!/bin/bash
set -euo pipefail

source .travis/fileServer.sh
.travis/travis_fold.sh downloadBuildData "Downloading build data" downloadBuildData
tree sync
