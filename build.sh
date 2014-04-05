#!/usr/bin/env bash

[ ! -e cabal.sandbox.config ] && cabal sandbox init
cabal install --disable-library-profiling --disable-executable-profiling\
              --disable-library-for-ghci  --disable-documentation\
              -fPIC --enable-shared --enable-executable-dynamic
