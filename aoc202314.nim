import aocd
import std/[strutils, sequtils, strformat, unittest]

type Grid = seq[string]

const
  Blank = '.'
  FixRock = '#'
  RollRock = 'O'
  MovedRock = 'X'

const example {.used.} = """
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
"""

proc transpose(grid: Grid): Grid =
  result = newSeqWith[string](grid[0].len, 'O'.repeat grid.len)
  for y in 0..grid.high:
    for x in 0..grid[0].high:
      result[x][y] = grid[y][x]

proc slideNorth(grid: var Grid): int =
  result = 0
  for x in 0..grid[0].high:
    var
      ystop = 0
      yroll = 0

    while true:
      # Find the next spot to roll a rock into.
      while ystop < grid.high:
        case grid[ystop][x]:
        of Blank:
          break
        of RollRock:
          inc ystop
        of FixRock:
          inc ystop
        of MovedRock:
          inc ystop
        else:
          echo fmt"ystop = {ystop} char = {grid[ystop][x]}"
          assert false

      yroll = ystop + 1

      # Find the next rock to roll, and maybe move up the stop cursor if we pass
      # a fixed rock.
      while yroll <= grid.high:
        case grid[yroll][x]:
        of Blank:
          inc yroll
        of RollRock:
          break
        of FixRock:
          ystop = yroll
          inc yroll
        else:
          echo fmt"yroll = {yroll} char = {grid[yroll][x]}"
          assert false

      # Check if we have fallen off.
      if yroll > grid.high or ystop > grid.high:
        break

      # Now roll the rock into place.

      if grid[yroll][x] == RollRock and grid[ystop][x] == Blank:
        grid[ystop][x] = MovedRock
        grid[yroll][x] = Blank
        inc ystop
        inc yroll

proc calculateWeight(grid: Grid): int =
  let height = grid.len
  result = 0
  for y in 0..grid.high:
    for x in 0..grid[0].high:
      case grid[y][x]:
      of RollRock:
        result += height - y
      of MovedRock:
        result += height - y
      else:
        discard

proc `$`(grid: seq[string]): string =
  for row in grid:
    result &= row
    result &= "\n"

day 14:
  let grid = input.strip.splitLines

  part 1:
    var grid = grid
    echo grid
    discard grid.slideNorth
    echo ""
    echo grid
    echo ""
    result = grid.calculateWeight

  part 2:
    result = 0

  # verifyPart(1, 30705)
  # verifyPart(2, 44615)
