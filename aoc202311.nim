import aocd
import std/[strutils, intsets, sequtils, unittest]

type
  Pos = object
    x, y: int

day 11:
  var
    stars: seq[Pos]
    popY: IntSet
    popX: IntSet
  for (y, line) in inputLines.pairs:
    for x, c in line:
      if c == '#':
        popY.incl y
        popX.incl x
        stars.add Pos(x: x, y: y)

  proc pairWiseDistances(expansion: int): int =
    result = 0
    for (i, sa) in stars.pairs:
      for j in i..stars.high:
        let sb = stars[j]
        result += abs(sb.x - sa.x) + abs(sb.y - sa.y)

        # Account for the universe expanding
        inc result, expansion * (min(sa.x, sb.x)..max(sa.x, sb.x)).countIt(it notin popX)
        inc result, expansion * (sa.y..sb.y).countIt(it notin popY)

  part 1:
    pairWiseDistances(1)

  part 2:
    pairWiseDistances(999_999)

  verifyPart(1, 9556896)
  verifyPart(2, 685038186836)
