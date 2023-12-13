import aocd
import std/[strutils, sequtils, unittest]

proc hasExactSmudgeCount(grid: seq[string], foldBefore: int, count: int): bool =
  let width = grid[0].len
  let height = grid.len
  var errors = 0

  for d in 1..grid.len:
    let y1 = foldBefore - d
    let y2 = foldBefore + d - 1

    # Keep going until we fall off either end
    if y1 notin 0..<height or y2 notin 0..<height:
      break

    for x in 0..<width:
      if grid[y1][x] != grid[y2][x]:
        inc errors
      if errors > count:
        return false
  return errors == count

iterator findFoldWithSmudges(grid: seq[string], count: int): int =
  ## Find a folding position with exact count of smudges.
  for fold in 1..grid.high:
    if hasExactSmudgeCount(grid, fold, count):
      yield fold

proc findFirstFoldWithSmudges(grid: seq[string], count: int, avoid: int): int =
  ## Find the first fold with exact smudge count, unless we are asked to avoid
  ## that specific fold.
  result = -1
  for fold in findFoldWithSmudges(grid, count):
    if fold != avoid:
      return fold

proc transpose(grid: seq[string]): seq[string] =
  result = newSeqWith[string](grid[0].len, 'O'.repeat grid.len)
  for y in 0..grid.high:
    for x in 0..grid[0].high:
      result[x][y] = grid[y][x]

day 13:
  let grids = input.strip.splitLines.groupByBlank
  var avoidV = newSeqWith[int](grids.len, int.low)
  var avoidH = newSeqWith[int](grids.len, int.low)

  part 1:
    result = 0
    for i, grid in grids.pairs:
      if (let aboveFold = findFirstFoldWithSmudges(grid, 0, -1); aboveFold > 0):
        avoidV[i] = aboveFold
        result += 100 * aboveFold
      elif (let leftOfFold = findFirstFoldWithSmudges(grid.transpose, 0, -1); leftOfFold > 0):
        avoidH[i] = leftOfFold
        result += leftOfFold

  part 2:
    result = 0
    for i, grid in grids.pairs:
      # Every mirror has _exactly_ one smudge, so find it in either the original
      # or transposed grid.
      # It should also cause the original fold to no longer be valid...
      if (let fold = findFirstFoldWithSmudges(grid, 1, avoidV[i]); fold != -1):
        result += 100 * fold
      elif (let fold = findFirstFoldWithSmudges(grid.transpose, 1, avoidH[i]); fold != -1):
        result += fold

  verifyPart(1, 30705)
  verifyPart(2, 44615)
