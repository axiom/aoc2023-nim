import aocd
import std/[strutils, sequtils, unittest]

const example1 {.used.} = """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"""

proc numbers(input: string): seq[int] =
  input.splitWhitespace.map(parseInt)

func calculateDiff(input: seq[int]): seq[int] =
  for i in 1..input.high:
    result.add(input[i] - input[i - 1])

func pyramid(input: seq[int]): seq[seq[int]] =
  result.add input
  while true:
    let diff = calculateDiff result[^1]
    if not diff.anyIt(it != 0):
      break
    result.add diff

day 9:
  let reports = inputLines.map(numbers)

  part 1:
    reports.foldl(a + b.pyramid.foldl(a + b[^1], 0), 0)

  part 2:
    reports.foldl(a + b.pyramid.mapIt(it[0]).foldr(a - b), 0)

  verifyPart(1, 1921197370)
  verifyPart(2, 1124)
