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


_print_diff () {
  local expected=$1
  local got=$1
}

_test_machine () {
  local machine=$1.json
  local input=$(cat $1.input)
  local output=$(cat $1.output)

  local res=$(./$BIN $machine $input 2>&1)
  [[ "$res" == "$output" ]] && echo "Ok!" || echo "Nop.."
}

[ -f $BIN ] && echo "Bin installed" || _build

# [ -f $TESTS_DIR ] && echo "Test machine file in $TEST_DIR" || _exit 1 "No tests found!"

# [ "$1" == "--diff" ]

for file in $TESTS_DIR*.json; do
  _test_machine "${file%.*}"
done
