import Intake.Core
import Intake.Job
import System.Exit
main = defaultMain defaultJob
  { jobCommand = "false"
  , jobExitCode = Just (ExitFailure 1)
  }
