#! /usr/bin/env bash

docker run \
  -v $(pwd)/../intake:/home/gusdev/intake \
  -v $(pwd)/../lovelace:/home/gusdev/lovelace \
  images.reesd.com/reesd/stack:7.8.4 \
  cabal install \
    lovelace/lovelace.cabal \
    intake/intake.cabal
