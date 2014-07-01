#!/bin/bash

# My applogies for this file; the differences in sed between OS X and Linux are
# minor but significant.  I would have liked to put this in the Makefile, but
# could not find any reasonable way to get newlines in a string.
NEWLINE=$'\n'

if [ $(uname) == Darwin ]; then
  sed -i '' "/extra-libraries/ c \\${NEWLINE}\
  \  extra-libraries:     HSrts-ghc$(ghc --numeric-version)" ExampleModule.cabal
else
  sed -i "/extra-libraries/ c \\${NEWLINE}\
\  extra-libraries:     HSrts-ghc$(ghc --numeric-version)" ExampleModule.cabal
fi
