# Intake - UNIX jobs orchestration

Intake runs process workflows (as in UNIX, not business process).

## Example workflows

- [workflows/workflow-sample-00.json](workflows/workflow-sample-00.json)
  The simplest workflow is the identity workflow: its input is its result.

## Tentative README (a.k.a. not implemented stuff)

### Running individual commands

`intake` can run multiple jobs arranged in workflows, but at its heart it is
just a simple tool to run commands. As a starter, consider running `echo
hello world` the `intake` way:

    > intake run -c echo hello world
    2e993bdb7b2d  2e993bdb7b2d8f01522e30cdfbb2a58f7ca663b5e54ba3f1b46b30305e70332a

Instead of printing "hello world" to stdout, `intake` will create a process to
run the command and print some ID. That ID can then be used to retrieve the
actual output of the command:

    > intake logs 2e993bdb7b2d
    2e993bdb7b2d @4000000051b609a41cf78cdc hello world

When the command takes time to complete, it is useful to probe the process
status:

    > intake status 2e993bdb7b2d
    2e993bdb7b2d Running.
    > intake status 2e993bdb7b2d
    2e993bdb7b2d Succeeded.

If you want to follow the output as the process progresses, you can give the
`-a` (as "attach") to `run`:

    > intake run -c -a echo hello world
    2e993bdb7b2d @4000000051b609a41cf78cdc hello world

Another useful option is `-t` to provide a timeout (in seconds):

    > intake run -c -a -t 1 echo hello world
    2e993bdb7b2d @4000000051b609a41cf78cdc hello world

    > intake run -c -a -t 0 echo hello world
    2e993bdb7b2d Timed out.

Individual commands can be given short names with the `define` sub-command:

    > intake define -c greetings echo hello world
    > intake run greetings
    2e993bdb7b2d  2e993bdb7b2d8f01522e30cdfbb2a58f7ca663b5e54ba3f1b46b30305e70332a

### Running workflows

In addition to running individual commands, `intake` can run multiple commands
arranged in workflows. For instance, it is possible to run two or more commands
sequentially or in parallel. If a command fails, `intake` can be instructed to
retry it a few times, or to continue with the rest of the workflow, ignoring
the failure.

We saw above that commands can be given on the command-line or aliased with the
`define` sub-command. Workflows can also be aliased but must be given by file:

    > intake run -f workflow.txt
    2e993bdb7b2d  2e993bdb7b2d8f01522e30cdfbb2a58f7ca663b5e54ba3f1b46b30305e70332a

or

    > intake define work workflow.txt
    > intake run work
    2e993bdb7b2d  2e993bdb7b2d8f01522e30cdfbb2a58f7ca663b5e54ba3f1b46b30305e70332a


## Tests

TODO.

~~~ {.haskell}
import Data.Aeson (object)
import Test.HUnit

import Intake.Commands (parseFile, run)
import Intake.Worker (successWorker)

main :: IO ()
main = do
  runTestTT $
    TestLabel "identity" (TestCase (do
      Right def <- parseFile "workflows/workflow-sample-00.json"
      -- TODO Use QuickCheck instead.
      let input = object []
      result <- run False (successWorker False) def input
      assertEqual "identity result" input result))
  return ()
~~~
