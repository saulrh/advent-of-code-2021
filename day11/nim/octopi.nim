import std/tables
import point
import std/strutils
import std/sequtils
import std/sugar
import std/deques
import std/sets

type Octopi = object
    data: Table[point.Point, int]
    steps*: int
    observed_flashes*: int

func `[]`*(o: Octopi, pt: point.Point): int =
  o.data[pt]

func topleft*(o: Octopi): point.Point =
  for pt, energy in o.data:
    result = min(pt, result)

func bottomright*(o: Octopi): point.Point =
  for pt, energy in o.data:
    result = max(pt, result)

func `$`*(o: Octopi): string =
  let br = o.bottomright
  var output = newSeqWith(br.row + 1, newSeq[string](br.col + 1))
  for pt, energy in o.data:
    output[pt.row][pt.col] = $energy
  var rows = newSeq[string]()
  for row in output:
    rows.add(join(row, ""))
  return join(rows, "\n")

func parse*(d: string): Octopi =
  var pt = point.zero
  for row in d.splitLines:
    for c in row:
      result.data[pt] = int(c) - int('0')
      pt.col += 1
    pt.col = 0
    pt.row += 1

func step*(o: var Octopi) =
  for energy in o.data.mvalues:
    energy += 1
  var flashed = initHashSet[point.Point]()
  var done = false
  while not done:
    done = true
    for pt, energy in o.data.pairs:
      if (pt notin flashed) and (energy > 9):
        done = false
        flashed.incl(pt)
        for n in pt.diagonal_neighbors:
          if n in o.data:
            o.data[n] += 1
  for pt in flashed:
    o.data[pt] = 0
  o.observed_flashes += len(flashed)
  o.steps += 1
  
func `==`*(a: Octopi, b: Octopi): bool =
  if a.topleft != b.topleft:
    return false
  if a.bottomright != b.bottomright:
    return false
  for pt, energy in a.data:
    if b[pt] != energy:
      return false
  for pt, energy in b.data:
    if a[pt] != energy:
      return false
  return true

func `synced`*(o: Octopi): bool =
  for energy in o.data.values:
    if energy != 0:
      return false
  return true

when isMainModule:
  var o = parse("123\n465\n")
  assert(o.topleft == point.zero)
  assert(o.bottomright == point.Point(row: 1, col: 2))
  o.step
  assert(o.observed_flashes == 0)
  assert(o == parse("234\n576"))
  o.step
  assert(o.observed_flashes == 0)
  assert(o == parse("345\n687"))
  o.step
  assert(o.observed_flashes == 0)
  assert(o == parse("456\n798"))
  o.step
  assert(o.observed_flashes == 2)
  assert(o == parse("689\n900"))
