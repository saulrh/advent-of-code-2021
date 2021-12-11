import octopi
import std/strutils
import std/strformat
import std/sugar

proc run_example(base: string, idx: int) =
  var before = octopi.parse(readFile(base % [$idx]))
  var after = octopi.parse(readFile(base % [$(idx + 1)]))
  let before_stepped = before.dup(step)
  assert(before_stepped == after, fmt"After evaluating {idx + 1} steps of {base}:" & "\n" & $before_stepped & "\n\n" & $after)

for idx in 0..1:
  run_example("../small_example_$1.txt", idx)

for idx in 0..1:
  run_example("../big_example_$1.txt", idx)

var example = octopi.parse(readFile("../big_example_0.txt"))
for i in 0..9:
  example.step
assert(example.observed_flashes == 204, $example.observed_flashes)

for _ in 10..99:
  example.step
assert(example.observed_flashes == 1656, $example.observed_flashes)

while not example.synced:
  example.step
assert(example.steps == 195)

echo("Big example passed")

var data = octopi.parse(readFile("../input.txt"))
for _ in 0..99:
  data.step
echo(fmt"Number of flashes after 100 steps: {data.observed_flashes}")


while not data.synced:
  data.step
echo(fmt"Input synced after {data.steps} steps")
