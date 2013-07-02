-- The test suite is test-and-coverage.sh:
-- Using the commands
--
-- > cabal clean
-- > cabal configure --enable-tests --enable-library-coverage
-- > cabal build && cabal test
--
-- would be pretty neat but the test suite is supposed to invoke the different
-- intake subcommands, generating multiple .tix files. I don't see how to do
-- that with just cabal (or combine the .tix files, or use something else than
-- `hpc markup` to generate HTML coverage files), so I will keep the
-- test-and-coverage.sh script.
module Main (main) where

main :: IO ()
main = return ()
