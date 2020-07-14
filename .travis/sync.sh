#!/bin/bash

mkdir -p sync/$TRAVIS_BUILD_NUMBER
touch sync/$TRAVIS_BUILD_NUMBER/${TRAVIS_BUILD_ID}.txt
ls
ls sync
RSYNC_PASSWORD="${VERCORS_RSYNC_PASSWORD}" rsync -avP --no-perms --no-owner --no-group ./sync/* rsync://travis@130.89.1.130:30303/volume
ls sync
