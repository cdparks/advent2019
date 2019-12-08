#!/bin/sh

set -eu

for i in $(ls inputs/ | sed -E 's/day([0-9]+)\.txt/\1/'); do
  for j in $(seq 1 2); do
    command="DAY=$i PART=$j stack exec advent < inputs/day$i.txt"
    echo "$command" && sh -c "$command" && printf "\n"
  done;
done;
