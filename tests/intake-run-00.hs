import Intake.Core
import Intake.Job
import System.Exit
main = defaultMain'
  [ defaultJob
  { jobCommand = "./dist/build/intake/intake"
  , jobArguments = words "run a"
  , jobStderr = Just ""
  , jobStdout = Just "{{SHORT_ID}}  {{ID}}\n"
  }
  , defaultJob
  { jobCommand = "sleep"
  , jobArguments = words "0.1"
  }
  , defaultJob
  { jobCommand = "./dist/build/intake/intake"
  , jobArguments = words "status {{$SHORT_ID}}"
  , jobStderr = Just ""
  , jobStdout = Just "WCompleted\n"
  }
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
