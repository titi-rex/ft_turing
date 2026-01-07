#!/usr/bin/env zsh

BIN=ft-turing
TESTS_DIR='./machines/tests/'

_build () {
  echo "Building $BIN"
  cabal install --installdir=.
  echo "Done!"
}

_exit () {
  [ -n $2 ] && echo "Error: $2"
  exit $1
}

[ -f $BIN ] && echo "Bin already installed" || _build

[ -f $TESTS_DIR ] && echo "Test machine file in $TEST_DIR" || _exit 1 "No tests found!"

for file in $TESTS_DIR*.json; do
  echo $file;
  echo $file.input;
done
