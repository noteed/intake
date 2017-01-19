#! /usr/bin/env bash

docker run \
  -v $(pwd)/../intake:/home/gusdev/intake \
  images.reesd.com/reesd/stack:7.8.4 \
  cabal install intake/intake.cabal
