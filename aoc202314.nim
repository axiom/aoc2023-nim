import aocd
import std/[strutils, sequtils, strformat, tables, unittest]

type Grid = seq[string]

const
  Blank = '.'
  Fixed = '#'
  Rolls = 'O'

func slideNorth(grid: var Grid) =
  for x in 0..grid[0].high:
    var
      stop = 0
      roll = 0

    while true:
      # Find the next spot to roll a rock into.
      while stop < grid.high:
        case grid[stop][x]:
        of Blank:
          break
        of Rolls:
          inc stop
        of Fixed:
          inc stop
        else:
          assert false

      roll = stop + 1

      # Find the next rock to roll, and maybe move up the stop cursor if we pass
      # a fixed rock.
      while roll <= grid.high:
        case grid[roll][x]:
        of Blank:
          inc roll
        of Rolls:
          break
        of Fixed:
          stop = roll
          inc roll
        else:
          assert false

      # Check if we have fallen off.
      if roll > grid.high or stop > grid.high:
        break

      # Now roll the rock into place.

      if grid[roll][x] == Rolls and grid[stop][x] == Blank:
        grid[stop][x] = Rolls
        grid[roll][x] = Blank
        inc stop
        inc roll

func slideSouth(grid: var Grid) =
  for x in 0..grid[0].high:
    var
      stop = grid.high
      roll = grid.high

    while true:
      # Find the next spot to roll a rock into.
      while stop > 0:
        case grid[stop][x]:
        of Blank:
          break
        of Rolls:
          dec stop
        of Fixed:
          dec stop
        else:
          assert false

      roll = stop - 1

      # Find the next rock to roll, and maybe move up the stop cursor if we pass
      # a fixed rock.
      while roll >= 0:
        case grid[roll][x]:
        of Blank:
          dec roll
        of Rolls:
          break
        of Fixed:
          stop = roll
          dec roll
        else:
          assert false

      # Check if we have fallen off.
      if roll < 0 or stop < 0:
        break

      # Now roll the rock into place.

      if grid[roll][x] == Rolls and grid[stop][x] == Blank:
        grid[stop][x] = Rolls
        grid[roll][x] = Blank
        dec stop
        dec roll

func slideEast(grid: var Grid) =
  for y in 0..grid.high:
    var
      stop = grid.high
      roll = grid.high

    while true:
      # Find the next spot to roll a rock into.
      while stop > 0:
        case grid[y][stop]:
        of Blank:
          break
        of Rolls:
          dec stop
        of Fixed:
          dec stop
        else:
          assert false

      roll = stop - 1

      # Find the next rock to roll, and maybe move up the stop cursor if we pass
      # a fixed rock.
      while roll >= 0:
        case grid[y][roll]:
        of Blank:
          dec roll
        of Rolls:
          break
        of Fixed:
          stop = roll
          dec roll
        else:
          assert false

      # Check if we have fallen off.
      if roll < 0 or stop < 0:
        break

      # Now roll the rock into place.

      if grid[y][roll] == Rolls and grid[y][stop] == Blank:
        grid[y][stop] = Rolls
        grid[y][roll] = Blank
        dec stop
        dec roll

func slideWest(grid: var Grid) =
  let high = grid[0].high
  for y in 0..grid.high:
    var
      stop = 0
      roll = 0

    while true:
      # Find the next spot to roll a rock into.
      while stop < high:
        case grid[y][stop]:
        of Blank:
          break
        of Rolls:
          inc stop
        of Fixed:
          inc stop
        else:
          assert false

      roll = stop + 1

      # Find the next rock to roll, and maybe move up the stop cursor if we pass
      # a fixed rock.
      while roll <= high:
        case grid[y][roll]:
        of Blank:
          inc roll
        of Rolls:
          break
        of Fixed:
          stop = roll
          inc roll
        else:
          assert false

      # Check if we have fallen off.
      if roll > high or stop > high:
        break

      # Now roll the rock into place.

      if grid[y][roll] == Rolls and grid[y][stop] == Blank:
        grid[y][stop] = Rolls
        grid[y][roll] = Blank
        inc stop
        inc roll

proc cycle(grid: var Grid, times: int) =
  for i in 0..<times:
    grid.slideNorth
    grid.slideWest
    grid.slideSouth
    grid.slideEast

proc cycle(grid: var Grid) = cycle(grid, 1)

proc calculateWeight(grid: Grid): int =
  result = 0
  for y in 0..grid.high:
    for x in 0..grid[0].high:
      if grid[y][x] == Rolls:
        result += grid.len - y

proc `$`(grid: seq[string]): string =
  for row in grid:
    result &= row & "\n"

func hashKey(grid: Grid): string = $grid

day 14:
  let grid = input.strip.splitLines

  part 1:
    var grid = grid
    grid.slideNorth
    grid.calculateWeight

  part 2:
    # This part introduces slides in the remaining directions, south, east and
    # west. The twist is that the sliding should be done 1000000000 times.
    # I don't think we can brute force that, maybe we can find a "cycle" and do
    # some calculations from that. Yeah, absolutly not brute force.
    #
    # Rotating the grid before each slide to not have to implement the other
    # sliding functions might be too expensive... Do I have to write sliding
    # functions for all directions? Meta-programming?
    var
      calcGrid = grid
      grid = grid
      seen = {grid.hashKey: 0}.toTable
    let
      cycleDetectionEffort = 1_000
      targetCycles = 1_000_000_000

    for i in 0..cycleDetectionEffort:
      cycle grid

      let gridKey = grid.hashKey

      if (let alreadySeen = gridKey in seen; alreadySeen):
        let cycleStart = seen[gridKey]
        let cycleLength = i - seen[gridKey]
        let remainder = (targetCycles - cycleStart) mod cycleLength
        cycle(calcGrid, cycleStart + remainder)
        return calcGrid.calculateWeight
      else:
        seen[gridKey] = i

    assert false

  verifyPart(1, 110407)
  verifyPart(2, 87273)
