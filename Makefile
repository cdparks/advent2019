MAKEFLAGS += --no-builtin-rules
.SUFFIXES:
.DEFAULT_GOAL := help

## Generate and test all solutions
.PHONY: solutions
solutions: opt
	@./solutions

day ?= 1
part ?= 1

## Generate solution for one day by setting [day=N] [part=N]
.PHONY: run
run: opt
	DAY=$(day) PART=$(part) stack exec advent < inputs/day$(day).txt

## Build optimized
.PHONY: opt
opt:
	stack build advent --pedantic --interleaved-output

## Build unoptimized
.PHONY: build
build:
	stack build advent --pedantic --fast --interleaved-output

## Animate Day 10 solution and save as asteroids.gif
.PHONY: asteroids
asteroids: opt
	DAY=10 PART=2 SAVE=asteroids.gif SCALE=12 DELAY=5 stack exec advent < inputs/day10.txt

## Animate Day 13 solution and save as breakout.gif
.PHONY: breakout
breakout: opt
	DAY=13 PART=2 SAVE=breakout.gif stack exec advent < inputs/day13.txt

## Update compiler, dependencies, and tools
.PHONY: update
update:
	$(MAKE) update.stack update.tools

## Set up the compiler and project dependencies
.PHONY: update.stack
update.stack:
	stack setup
	stack build --fast --test --no-run-tests --dependencies-only

## Install additional tooling (e.g. HLint, brittany, etc)
.PHONY: update.tools
update.tools:
	stack build --copy-compiler-tool \
	  apply-refact \
	  fast-tags \
	  hlint \
	  stylish-haskell \
	  brittany

## Clean project
.PHONY: clean
clean:
	stack clean advent

# Produce help output for Makefile
#
# Doc blocks are signified with ##
#
# Sections can be added with ## -- Section --
#
# source: https://gist.github.com/prwhite/8168133#gistcomment-2749866
#
.PHONY: help
help:
	@printf "Usage\n";
	@awk '{ \
	  if ($$0 ~ /^.PHONY: [a-zA-Z\/\-\.\_0-9]+$$/) { \
	    helpCommand = substr($$0, index($$0, ":") + 2); \
	    if (helpMessage) { \
	      printf "\033[36m%-20s\033[0m %s\n", \
	        helpCommand, helpMessage; \
	      helpMessage = ""; \
	    } \
	  } else if ($$0 ~ /^[a-zA-Z\/\-\.\_0-9.]+:/) { \
	    helpCommand = substr($$0, 0, index($$0, ":")); \
	    if (helpMessage) { \
	      printf "\033[36m%-20s\033[0m %s\n", \
	        helpCommand, helpMessage; \
	      helpMessage = ""; \
	    } \
	  } else if ($$0 ~ /^##/) { \
	    if (helpMessage) { \
	      helpMessage = helpMessage"\n                     "substr($$0, 3); \
	    } else { \
	      helpMessage = substr($$0, 3); \
	    } \
	  } else { \
	    if (helpMessage) { \
	      print "\n                     "helpMessage"\n" \
	    } \
	    helpMessage = ""; \
	  } \
	}' \
	$(MAKEFILE_LIST)
