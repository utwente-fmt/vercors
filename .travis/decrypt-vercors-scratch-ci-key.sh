#!/bin/bash

# Decrypt file
openssl aes-256-cbc -K $encrypted_d2cc1ee25cc3_key -iv $encrypted_d2cc1ee25cc3_iv -in vercors_buildbot.enc -out ./vercors_buildbot -d
# Set appropriate permissions for use as ssh identity file
chmod 600 vercors_buildbot
