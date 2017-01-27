#! /bin/bash

# TODO I thought markdown-unlit was already part of the stack image.

docker run -it \
  -v $(pwd)/../intake:/home/gusdev/intake \
  -v $(pwd)/../lovelace:/home/gusdev/lovelace \
  images.reesd.com/reesd/stack:7.8.4 \
  sh -c 'cabal install markdown-unlit ; cd intake ;
  ghci -i../lovelace -idist/build/autogen -pgmL markdown-unlit README.lhs'
