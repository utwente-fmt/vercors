#!/bin/bash

export VERCORS_FILE_SERVER_IP="130.89.1.130"
export VERCORS_FILE_SERVER_PORT="873"
export VERCORS_FILE_SERVER_ADDR="${VERCORS_FILE_SERVER_IP}:${VERCORS_FILE_SERVER_PORT}"

uploadBuildData() {
  RSYNC_PASSWORD="${VERCORS_RSYNC_PASSWORD}" rsync -avP --no-perms --no-owner --no-group \
    ./sync/${TRAVIS_BUILD_NUMBER}/* \
    rsync://travis@${VERCORS_FILE_SERVER_ADDR}/volume/${TRAVIS_BUILD_NUMBER}
}

downloadBuildData() {
  mkdir -p sync/${TRAVIS_BUILD_NUMBER}
  RSYNC_PASSWORD="${VERCORS_RSYNC_PASSWORD}" rsync -avP --no-perms --no-owner --no-group \
    rsync://travis@${VERCORS_FILE_SERVER_ADDR}/volume/${TRAVIS_BUILD_NUMBER}/*
    sync/${TRAVIS_BUILD_NUMBER}
}

clearBuildData() {
  # Ensure build data dir is empty
  rm -r sync/${TRAVIS_BUILD_NUMBER}
  mkdir -p sync/${TRAVIS_BUILD_NUMBER}
  RSYNC_PASSWORD="${VERCORS_RSYNC_PASSWORD}" rsync -avP --no-perms --no-owner --no-group --delete \
    ./sync/${TRAVIS_BUILD_NUMBER}/* \
    rsync://travis@${VERCORS_FILE_SERVER_ADDR}/volume/${TRAVIS_BUILD_NUMBER}
}

export -f uploadBuildData
export -f downloadBuildData
export -f clearBuildData