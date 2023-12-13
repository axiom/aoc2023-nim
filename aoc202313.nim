import aocd
import std/[strformat, strutils, sequtils, unittest]

type
  Pos = object
    y, x: int

const BadPos = Pos(y: -1, x: -1)

iterator findSmudges(grid: seq[string], foldBefore: int): Pos =
  ## Find the location of all "smudges" i.e., positions that do not reflect
  ## perfectly in their counterpart.

  var errors = 0
  let width = grid[0].len
  let height = grid.len

  for d in 1..grid.len:
    let y1 = foldBefore - d
    let y2 = foldBefore + d - 1

    # Keep going until we fall off either end
    if y1 notin 0..<height or y2 notin 0..<height:
      break

    for x in 0..<width:
      let c1 = grid[y1][x]
      let c2 = grid[y2][x]
      if c1 != c2 and c1 != '?' and c2 != '?':
        yield Pos(y: y1, x: x)
        inc errors

iterator findFoldWithSmudges(grid: seq[string], count: int): (int, seq[Pos]) =
  ## Find a folding position with exactly count smudges. The fold line and the
  ## smudge positions are returned.
  for fold in 1..grid.high:
    var smudgeCount = 0
    var smudges: seq[Pos]
    for smudge in findSmudges(grid, fold):
      smudges.add smudge
      inc smudgeCount
    if smudgeCount == count:
      yield (fold, smudges)

proc firstPerfectFold(grid: seq[string]; unless: int): int =
  ## Find the first folding position, as long as it isnt unless, that reflects
  ## perfectly, and return the number of lines above the fold.
  result = -1
  for (fold, _) in findFoldWithSmudges(grid, 0):
    if result != unless:
      return fold

proc findFoldWithSingleSmudge(grid: seq[string], unless: int): (int, Pos) =
  ## Find a single smudge in the grid, and return its position.
  result = (-1, BadPos)
  for (fold, smudges) in findFoldWithSmudges(grid, 1):
    if fold != unless:
      return (fold, smudges[0])

func `*`(s: string, repeats: int): string =
  if repeats <= 0: return s
  return s & (s * (repeats - 1))

proc transpose(grid: seq[string]): seq[string] =
  result = newSeqWith[string](grid[0].len, "O" * grid.high)
  for y in 0..grid.high:
    for x in 0..grid[0].high:
      result[x][y] = grid[y][x]

proc copy(grid: seq[string]): seq[string] =
  result = newSeqWith[string](grid.len, "O" * grid[0].high)
  for y in 0..grid.high:
    for x in 0..grid[0].high:
      result[y][x] = grid[y][x]

proc `$`(grid: seq[string]): string =
  for y in 0..grid.high:
    result &= grid[y]
    if y != grid.high:
      result &= "\n"

const testExample {.used.} = """
####
####
####
####
""".strip.splitLines

check testExample.findFoldWithSmudges(1).toSeq.len == 0
check testExample.findFoldWithSmudges(0).toSeq.len == 3

const testExample2 {.used.} = """
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
#.##..##.
#.#.##.#.
""".strip.splitLines

const example1 {.used.} = """
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
"""

day 13:
  let grids = input.strip.splitLines.groupByBlank
  # keep track of folds to avoid.
  var unlessV = newSeqWith[int](grids.len, int.low)
  var unlessH = newSeqWith[int](grids.len, int.low)

  part 1:
    result = 0
    for i, grid in grids.pairs:
      if (let aboveFold = firstPerfectFold(grid, unlessV[i]); aboveFold > 0):
        unlessV[i] = aboveFold
        result += 100 * aboveFold
      elif (let leftOfFold = firstPerfectFold(grid.transpose, unlessH[i]); leftOfFold > 0):
        unlessH[i] = leftOfFold
        result += leftOfFold

  part 2:
    result = 0
    for i, grid in grids.pairs:
      # Every mirror has _exactly_ one smudge, so find it in either the original
      # or transposed grid, then correct it and proceed with the calculations?

      if (let (fold, smudge) = findFoldWithSingleSmudge(grid, unlessV[i]); fold != -1):
        result += 100 * fold
      elif (let (fold, _) = findFoldWithSingleSmudge(grid.transpose, unlessH[i]); fold != -1):
        result += fold

  verifyPart(1, 30705)
  verifyPart(2, 44615)
