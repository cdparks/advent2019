#!/bin/sh

set -eu

mkdir -p .solutions
rm -f .solutions/*

for i in $(seq 1 25); do
  input="inputs/day$i.txt"
  for j in $(seq 1 2); do
    output="day$i-$j.txt"
    solution=".solutions/$output"
    expected="expected/$output"
    if [ -f "$input" ] && [ -f "$expected" ]; then
      command="DAY=$i PART=$j stack exec advent < $input | tee $solution && diff $expected $solution"
      echo "$command" && sh -c "$command" && printf "\n"
    fi;
  done;
done;
