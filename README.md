# Advent of Code 2019

Solutions for [Advent of Code 2019] in [Haskell]

## Help

Run `make` without a target to show help

```console
make
Usage
solutions             Generate and test all solutions
run                   Generate solution for one day by setting [day=N] [part=N]
opt                   Build optimized
build                 Build unoptimized
asteroids             Animate Day 10 solution and save as asteroids.gif
breakout              Animate Day 13 solution and save as breakout.gif
update                Update compiler, dependencies, and tools
update.stack          Set up the compiler and project dependencies
update.tools          Install additional tooling (e.g. HLint, brittany, etc)
clean                 Clean project
```

## Gallery

### Day 10 - Asteroids

![Asteroids]

The laser can only destroy one asteroid at a time. If a destroyed
asteroid was occluding other asteroids, we have to sweep back around
to target them.

### Day 13 - Breakout

![Breakout]

Starting from the beginning of the animation, we intentionally start
dropping frames until we're dropping up to 60 at a time in the middle.
We slowly reduce the number of dropped frames until we land on the
frame where the final block is destroyed.

[Advent of Code 2019]: https://adventofcode.com/2019
[Haskell]: https://www.haskell.org
[Asteroids]: images/asteroids.gif
[Breakout]: images/breakout.gif
