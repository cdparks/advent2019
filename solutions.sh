#!/bin/sh

set -eu

mkdir -p .solutions
rm -f .solutions/*

for input in $(find inputs/ -name '*.txt' | sort -n); do
  i=$(echo "$input" | sed -E 's/.+day([0-9]+)\.txt/\1/')
  for j in $(seq 1 2); do
    output=day$i-$j.txt
    solution=.solutions/$output
    expected=expected/$output
    command="DAY=$i PART=$j stack exec advent < $input | tee $solution && diff $expected $solution"
    echo "$command" && sh -c "$command" && printf "\n"
  done;
done;
