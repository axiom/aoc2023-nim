import aocd
import std/[deques, strformat, strscans, strutils, sequtils, unittest]

proc reflectUpDown(grid: seq[string], foldAt: int): bool =
  if foldAt <= 0 or foldAt >= grid.len:
    return false

  let height = grid.len
  let halfHeight = height div 2

  # echo fmt"reflectUpDown: foldAt={foldAt}, height={height}, halfHeight={halfHeight}"

  result = true
  for distance in 1..halfHeight:
    let y1 = foldAt - distance
    let y2 = foldAt + distance - 1
    # echo fmt"reflectUpDown: distance={distance}, y1={y1}, y2={y2}"
    if y1 notin 0..<height or y2 notin 0..<height:
      # echo fmt"reflectUpDown: out of bounds"
      break

    # echo fmt"reflectUpDown: grid[y1]={grid[y1]}, grid[y2]={grid[y2]}"
    if grid[y1] != grid[y2]:
      # echo fmt"reflectUpDown: mismatch"
      result = false
      break

func `*`(s: string, repeats: int): string =
  if repeats <= 0: return s
  return s & (s * (repeats - 1))

proc transpose(grid: seq[string]): seq[string] =
  result = newSeqWith[string](grid[0].len, "." * grid.len)
  for y in 0..grid.high:
    for x in 0..grid[y].high:
      result[x][y] = grid[y][x]

proc `$`(grid: seq[string]): string =
  for y in 0..grid.high:
    result &= grid[y]
    if y != grid.high:
      result &= "\n"

const testExample {.used.} = """
##
##
##
""".strip.splitLines

check reflectUpDown(testExample, 2) == true
check reflectUpDown(testExample.transpose, 1) == true

const testExample2 {.used.} = """
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.
""".strip.splitLines

check reflectUpDown(testExample2, 4) == true
check reflectUpDown(testExample2.transpose, 5) == true

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

  part 1:
    result = 0
    for grid in grids:
      # echo ""
      # echo grid
      var gridResult = 0

      for foldAt in 0..grid.high:
        if reflectUpDown(grid, foldAt):
          gridResult += 100 * foldAt
          break

      let gridd = grid.transpose
      for foldAt in 0..gridd.high:
        if reflectUpDown(gridd, foldAt):
          gridResult += foldAt
          break

      result += gridResult

  part 2:
    0

  # 30619 is too low
  verifyPart(1, 30705)
  verifyPart(2, 0)
