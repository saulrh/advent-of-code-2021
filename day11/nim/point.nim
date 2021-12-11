import std/hashes
import std/strformat
import std/sequtils
import std/algorithm
import std/sets

type
  Point* = object
    row*: int
    col*: int

const up* = Point(row: -1, col: 0)
const down* = Point(row: 1, col: 0)
const left* = Point(row: 0, col: -1)
const right* = Point(row: 0, col: 1)
const zero* = Point(row: 0, col: 0)

func hash*(pt: Point): Hash =
  result = pt.row.hash !& pt.col.hash
  result = !$result

func min*(pta: Point, ptb: Point): Point =
  result.row = min(pta.row, ptb.row)
  result.col = min(pta.col, ptb.col)
  
func max*(pta: Point, ptb: Point): Point =
  result.row = max(pta.row, ptb.row)
  result.col = max(pta.col, ptb.col)

func `+`*(pta: Point, ptb: Point): Point =
  result.row = pta.row + ptb.row
  result.col = pta.col + ptb.col

func `*`*(pta: Point, ptb: Point): Point =
  result.row = pta.row * ptb.row
  result.col = pta.col * ptb.col
  
func `*`*(pta: Point, b: int): Point =
  result.row = pta.row * b
  result.col = pta.col * b
  
func `-`*(pta: Point, ptb: Point): Point =
  result.row = pta.row - ptb.row
  result.col = pta.col - ptb.col
  
func `==`*(pta: Point, ptb: Point): bool =
  (pta.row == ptb.row) and (pta.col == ptb.col)

func `$`*(pt: Point): string =
  fmt"<{pt.row}, {pt.col}>"


iterator neighbors*(pt: Point): Point =
  yield pt + up
  yield pt + down
  yield pt + left
  yield pt + right

iterator diagonals*(pt: Point): Point =
  yield pt + up + left
  yield pt + up + right
  yield pt + down + left
  yield pt + down + right

iterator diagonal_neighbors*(pt: Point): Point =
  for n in pt.neighbors:
    yield n
  for n in pt.diagonals:
    yield n

when isMainModule:
  let a = Point(row: 1, col: 3)
  let b = Point(row: 2, col: 4)
  let c = Point(row: 3, col: 7)
  let d = Point(row: -1, col: 5)
  doAssert(a == a)
  doAssert(a != b)
  doAssert(a + b == c)
  doAssert(c - b == a)
  doAssert(min(a, d) == Point(row: -1, col: 3))
  doAssert(max(a, d) == Point(row: 1, col: 5))

  const against = toHashSet([
    Point(row: 0, col: 2),
    Point(row: 0, col: 3),
    Point(row: 0, col: 4),
    Point(row: 1, col: 2),
    Point(row: 1, col: 4),
    Point(row: 2, col: 2),
    Point(row: 2, col: 3),
    Point(row: 2, col: 4),
  ])
  let diags = a.diagonal_neighbors.toSeq.toHashSet
  doAssert(diags == against, fmt"{against}  {diags}")
