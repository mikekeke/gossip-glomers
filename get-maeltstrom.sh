#!/bin/bash

set -euxo pipefail

VERSION=v0.2.4
TARGET=maelstrom.tar.bz2

rm -rf maelstrom
wget https://github.com/jepsen-io/maelstrom/releases/download/${VERSION}/${TARGET}
tar -xvjf ${TARGET}
rm -rf maelstrom.tar.bz2
# cp Makefile-ms-template maelstrom/Makefile
