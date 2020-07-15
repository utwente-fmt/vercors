#!/bin/bash

export VERCORS_FILE_SERVER_IP="130.89.1.130"
export VERCORS_FILE_SERVER_PORT="873"
export VERCORS_FILE_SERVER_ADDR="${VERCORS_FILE_SERVER_IP}:${VERCORS_FILE_SERVER_PORT}"

uploadBuildData() {
  # Use \* to prevent * from expanding to dir contents. Instead, we just want to pass the literal with * to rsync
  # Otherwise the one file in the directory is uploaded as the file ${TRAVIS_BUILD_NUMBER}, which instead should become a folder
  RSYNC_PASSWORD="${VERCORS_RSYNC_PASSWORD}" rsync -avP --no-perms --no-owner --no-group \
    sync/${TRAVIS_BUILD_NUMBER} \
    rsync://travis@${VERCORS_FILE_SERVER_ADDR}/volume
}

downloadBuildData() {
  mkdir -p sync/${TRAVIS_BUILD_NUMBER}
  RSYNC_PASSWORD="${VERCORS_RSYNC_PASSWORD}" rsync -avP --no-perms --no-owner --no-group \
    rsync://travis@${VERCORS_FILE_SERVER_ADDR}/volume/${TRAVIS_BUILD_NUMBER} \
    sync
}

clearBuildData() {
  # Ensure build data dir is empty
  rm -r sync/${TRAVIS_BUILD_NUMBER}
  mkdir -p sync/${TRAVIS_BUILD_NUMBER}
  RSYNC_PASSWORD="${VERCORS_RSYNC_PASSWORD}" rsync -avP --no-perms --no-owner --no-group --delete \
    ./sync/${TRAVIS_BUILD_NUMBER} \
    rsync://travis@${VERCORS_FILE_SERVER_ADDR}/volume
}

export -f uploadBuildData
export -f downloadBuildData
export -f clearBuildData