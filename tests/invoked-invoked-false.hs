import Intake.Core
import Intake.Job
import System.Exit
main = defaultMain defaultJob
  { jobCommand = "./dist/build/intake/intake"
  , jobArguments = words "run -f tests/invoked-false.txt"
  , jobStderr = Just
    "The command exit code is not Just ExitSuccess.\n\
    \The command stderr is:\n\n"
  , jobExitCode = Just (ExitFailure 1)
  }
