import aocd
import std/[strutils, sequtils, re, unittest]

const example1 = """
Time:      7  15   30
Distance:  9  40  200
"""

func extractNumbers(input: string): seq[int] =
  input.findAll(re"\d+").map(parseInt)

func wins(t, d: int): int =
  result = 1
  var ways = 0
  for w in 1..t:
    if d < w * (t - w):
      ways.inc
  result *= ways

func solve(nums: seq[seq[int]]): int =
  let times = nums[0]
  let distances = nums[1]
  result = 1
  for i in 0..times.high:
    result *= wins(times[i], distances[i])

day 6:
  part 1:
    inputInts().solve

  part 2:
    inputLines.mapIt(it.filter(isDigit).join.extractNumbers).solve

  verifyPart(1, 219849)
  verifyPart(2, 29432455)
