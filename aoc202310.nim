import aocd
import std/[deques, strutils, math, sets, strscans, tables, sequtils, unittest]

const example1 {.used.} = """
.....
.S-7.
.|.|.
.L-J.
.....
"""

const example2 {.used.} = """
..F7.
.FJ|.
SJ.L7
|F--J
LJ...
"""

type
  Pos = object
    r, c: int

day 10:
  let
    grid = input.strip.splitLines
    width = grid[0].len
    height = grid.len
    start = block:
      var start = Pos(r: 0, c: 0)
      for r in 0 ..< height:
        for c in 0 ..< width:
          if grid[r][c] == 'S':
            start = Pos(r: r, c: c)
      start

  part 1:
    var steps = 0
    var cur = start
    var seen: HashSet[Pos]
    seen.incl start

    while true:
      var me = grid[cur.r][cur.c]
      var north = if cur.r == 0: '.' else: grid[cur.r - 1][cur.c]
      var south = if cur.r == grid.high: '.' else: grid[cur.r + 1][cur.c]
      var east = if cur.c == grid[0].high: '.' else: grid[cur.r][cur.c + 1]
      var west = if cur.c == 0: '.' else: grid[cur.r][cur.c - 1]

      var northP = Pos(r: cur.r - 1, c: cur.c)
      var southP = Pos(r: cur.r + 1, c: cur.c)
      var eastP = Pos(r: cur.r, c: cur.c + 1)
      var westP = Pos(r: cur.r, c: cur.c - 1)

      inc steps

      # Can I go north?
      if   me in "J|LS" and north in "7|F" and northP notin seen:
        cur = northP
      elif me in "7|FS" and south in "J|L" and southP notin seen:
        cur = southP
      elif me in "F-LS" and east in "J-7" and eastP notin seen:
        cur = eastP
      elif me in "J-7S" and west in "F-L" and westP notin seen:
        cur = westP
      else:
        break

      seen.incl cur

    steps div 2

  part 2:
    0

  verifyPart(1, 6907)
  verifyPart(2, 0)
