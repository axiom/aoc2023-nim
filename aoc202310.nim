import aocd
import std/[os, deques, strutils, math, sets, strscans, tables, sequtils, unittest]

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

const example3 {.used.} = """
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
"""

const example4 {.used.} = """
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
"""

const example5 {.used.} = """
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
"""

type
  Pos = object
    r, c: int

# Replace ascii pipe chars with unicode single box drawing characters.
proc fancy(c: char): string =
  case c:
    of '|': "║"
    of '-': "═"
    of 'F': "╔"
    of '7': "╗"
    of 'J': "╝"
    of 'L': "╚"
    else: $c

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
    var
      cur = start
      steps: int
      path: seq[Pos]
      loop: HashSet[Pos]

    path.add cur
    loop.incl start

    while true:
      var
        me = grid[cur.r][cur.c]

        north = if cur.r == 0: '.' else: grid[cur.r - 1][cur.c]
        south = if cur.r == grid.high: '.' else: grid[cur.r + 1][cur.c]
        east = if cur.c == grid[0].high: '.' else: grid[cur.r][cur.c + 1]
        west = if cur.c == 0: '.' else: grid[cur.r][cur.c - 1]

        northP = Pos(r: cur.r - 1, c: cur.c)
        southP = Pos(r: cur.r + 1, c: cur.c)
        eastP = Pos(r: cur.r, c: cur.c + 1)
        westP = Pos(r: cur.r, c: cur.c - 1)

      inc steps

      # Can I go north?
      if   me in "J|LS" and north in "7|F" and northP notin loop:
        cur = northP
      elif me in "7|FS" and south in "J|L" and southP notin loop:
        cur = southP
      elif me in "F-LS" and east in "J-7" and eastP notin loop:
        cur = eastP
      elif me in "J-7S" and west in "F-L" and westP notin loop:
        cur = westP
      else:
        break

      path.add cur
      loop.incl cur

    result = 0
    for r in 0 ..< height:
      var crossings = 0
      var p = '.'
      for c in 0 ..< width:
        var g = grid[r][c]
        let onPath = Pos(r: r, c: c) in path

        if onPath:
          if   p == '.' and g == '|':
            inc crossings
          elif p == '|' and g == '|':
            inc crossings
          elif p == '7' and g == '|':
            inc crossings
          elif p == 'J' and g == '|':
            inc crossings
          elif p == 'L' and g == '7':
            inc crossings
          elif p == 'F' and (g == 'J' or g == 'S'):
            inc crossings

          if g != '-':
            p = g
          stdout.write fancy(g)
        else:
          if crossings mod 2 == 1:
            stdout.write "I"
            inc result
          else:
            if g == '.':
              stdout.write "O"
            else:
              stdout.write " "

      echo ""

  verifyPart(1, 6907)
  # 123 is too low
  # 553 is too high
  verifyPart(2, 541)
