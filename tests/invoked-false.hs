import Intake.Job
import System.Exit
main = defaultMain defaultInvoke
  { inCommand = "false"
  , inExitCode = Just (ExitFailure 1)
  }
