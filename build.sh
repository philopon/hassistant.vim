#!/usr/bin/env bash

[ ! -e cabal.sandbox.config ] && cabal sandbox init

if [ "$1" == "develop" ]; then
  echo develop
  cabal install -fPIC --enable-shared --enable-executable-dynamic --gcc-option=-fPIC
else
  cabal install --disable-library-profiling --disable-executable-profiling \
                --disable-library-for-ghci  --disable-documentation -fPIC  \
                --enable-shared --enable-executable-dynamic -fshared       \
                 --ghc-option=-fPIC --gcc-option=-fPIC
fi
