switch("verbosity", "0")
switch("hints", "off")

when defined(testing) :
  switch("verbosity", "1")
  switch("hints", "on")

task build, "build project":
  exec "nim c part1.nim"

task run, "build, test, and run project":
  exec "nim c -r part1.nim"

task check_all, "invoke check on whole project":
  exec "nim check point.nim"
  exec "nim check octopi.nim"
  exec "nim check part1.nim"

task test, "run non-program files for basic testing":
  exec "nim c -r point.nim"
  exec "nim c -r octopi.nim"

task all, "do everything":
  exec "nim check_all"
  exec "nim build"
  exec "nim test"
  exec "nim run"

task watch, "watch files and rerun on modification":
  exec "fd -e nim | entr -c nim all"
