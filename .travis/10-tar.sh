#!/bin/bash
sudo mv /bin/tar /bin/tar-orig
sudo mv travis_tar.sh /bin/tar
sudo chmod +x /bin/tar