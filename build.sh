#!/usr/bin/env bash

cabal sandbox init
cabal install --disable-library-profiling --disable-executable-profiling --disable-library-for-ghci --disable-documentation
