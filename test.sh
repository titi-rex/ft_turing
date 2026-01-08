#!/usr/bin/env zsh

STYLE_ITALIC="\e[3m"
STYLE_BOLD="\e[1m"
STYLE_RED="\e[0;31m"
STYLE_GREEN="\e[0;32m"
STYLE_RESET="\e[0m"


# CLI
diff=false
cmd=false
help=false

get_flags() {
  while ((#)) do
    case $1 in
      -d|--diff)   shift; diff=true;;
      -h|--help)  shift; help=true;;
      -s|--show-command)  shift; cmd=true;;
      -*) _exit 1 "Error: unknown option: $1";;
      *)  break;;
    esac
  done
  typeset -r diff cmd
}

_exit () {
  [ -n $2 ] && echo "$2"
  exit $1
}

_usage () {
  echo "./test.sh [-d | --diff] [-s | --show-command]
  Put your test file in machines/tests
    - test_machine1.json
    - test_machine1.conf"
  exit 0
}


# Prerequisite
BIN=ft-turing
TESTS_DIR='tests'

build () {
  echo "Building $BIN"
  cabal install --installdir=.
  echo "Done!"
}

print_success () {
  local sep=" --> "
  local color=$STYLE_GREEN
  local message="Ok!"

  [[ "$1" == "n" ]] && color=$STYLE_RED && message="Nop..."

  echo " $STYLE_BOLD$sep$STYLE_RESET $color$message$STYLE_RESET"
}


# Tests

print_diff () {
  [[ $diff == "false" ]] && print_success n && return

  print_success n
  diff --color=always <(echo "$1") <(echo "$2")
}

test_machine () {
  local machine=$1.json
  local input=$(cat $1.conf | jq -r .input)
  local output=$(cat $1.conf | jq -r .output)

  echo -ne "${STYLE_BOLD}Test:$STYLE_RESET $STYLE_ITALIC${1##*/}$STYLE_RESET"

  local res=$(./$BIN $machine $input 2>&1)

  [[ $cmd == "true" ]] && echo -n " with $STYLE_ITALIC'./$BIN $machine $input'$STYLE_RESET"
  [[ "$res" == "$output" ]] && print_success || print_diff $output $res
}


# Main

[ -f $BIN ] || build
[ -d $TESTS_DIR ] && echo "Test files in '$STYLE_BOLD$TESTS_DIR$STYLE_RESET'" || _exit 1 "Error: test dir '$STYLE_BOLD$TESTS_DIR$STYLE_RESET' not found!"

get_flags $@

[[ $help == "true" ]] && _usage

for file in $TESTS_DIR/**/*.json; do
  test_machine "${file%.*}"
done
