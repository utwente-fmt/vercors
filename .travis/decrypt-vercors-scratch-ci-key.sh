#!/bin/bash

# Decrypt file
openssl aes-256-cbc -K $encrypted_d2cc1ee25cc3_key -iv $encrypted_d2cc1ee25cc3_iv -in $TRAVIS_BUILD_DIR/vercors_buildbot.enc -out $TRAVIS_BUILD_DIR/vercors_buildbot -d
# Set appropriate permissions for use as ssh identity file
chmod 600 $TRAVIS_BUILD_DIR/vercors_buildbot
