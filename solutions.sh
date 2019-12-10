#!/bin/sh

set -eu

mkdir -p .solutions
rm -f .solutions/*

for i in $(ls inputs/ | sed -E 's/day([0-9]+)\.txt/\1/' | sort -n); do
  for j in $(seq 1 2); do
    command="DAY=$i PART=$j stack exec advent < inputs/day$i.txt | tee .solutions/day$i-$j.txt && diff expected/day$i-$j.txt .solutions/day$i-$j.txt"
    echo "$command" && sh -c "$command" && printf "\n"
  done;
done;
