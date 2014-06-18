import Intake.Core
import Intake.Job
import System.Exit
main = defaultMain'
  [
  -- Run `intake run a` and capture its output as SHORT_ID and ID.
  defaultJob
  { jobCommand = "./dist/build/intake/intake"
  , jobArguments = words "run a"
  , jobStderr = Just ""
  , jobStdout = Just "{{SHORT_ID}}  {{ID}}\n"
  }
  -- Wait a bit.
  , defaultJob
  { jobCommand = "sleep"
  , jobArguments = words "0.1"
  }
  -- Run `intake status $SHORT_ID` with the captured SHORT_ID.
  , defaultJob
  { jobCommand = "./dist/build/intake/intake"
  , jobArguments = words "status {{$SHORT_ID}}"
  , jobStderr = Just ""
  , jobStdout = Just "WCompleted\n"
  }
  -- Run `intake show $SHORT_ID` with the captured SHORT_ID.
  -- This checks the stdout by comparing the captured ID.
  , defaultJob
  { jobCommand = "./dist/build/intake/intake"
  , jobArguments = words "show {{$SHORT_ID}}"
  , jobStderr = Just ""
  , jobStdout = Just
  "workflow: Right a\n\
  \id: {{$ID}}\n\
  \arguments: []\n\
  \state: #0 echo [\"a\"](Completed)\n\n"
  }
  ]
