#!/bin/bash
set -euo pipefail

source .travis/fileServer.sh
.travis/travis_fold.sh downloadBuildData "Downloading Vercors .deb file and coverage files" downloadBuildData
tree sync

# Extract deb so sonarcloud can get to class files inside .jar files
mkdir deb
dpkg-deb --extract sync/${TRAVIS_BUILD_NUMBER}/Vercors*.deb deb
tree -d deb

./.travis/travis_fold.sh checkstyle "Checkstyle" "checkstyle -c /google_checks.xml src hre col parsers -f xml > checkstyle.xml"

echo "TRAVIS_SECURE_ENV_VARS=${TRAVIS_SECURE_ENV_VARS}";
if [ "${TRAVIS_SECURE_ENV_VARS}" == "false" ]; then
echo;
echo "The check is running for a pull request for an external repo. At the moment Travis does not support running Sonar for external repositories. The build will fail.";
fi;

./.travis/travis_fold.sh sonar "Sonar" "sonar-scanner"

./.travis/clearBuildData.sh
