#!/bin/bash
set -ev

make test
make check
sudo make install
make ex 
if [ -n "$(git branch | egrep '^\* (release|master)')" ] ; then
	MODES=online make test
fi
