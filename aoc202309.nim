import aocd
import std/[strutils, re, strscans, tables, sequtils, unittest]

const example1 = """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"""

proc extractNumbers(input: string): seq[int] =
  input.findAll(re"-?\d+").map(parseInt)

day 9:
  let numberLines = input.strip.splitLines.map(extractNumbers)

  part 1:
    result = 0
    for nums in numberLines:
      var stack = @[nums]

      block buildup:
        var keepGoing = true
        while keepGoing:
          let line = stack[^1]
          var diff: seq[int]
          keepGoing = false
          for i in 1..line.high:
            diff.add(line[i] - line[i - 1])
            keepGoing = true
          if keepGoing:
            stack.add diff

      var x = 0
      while stack.len > 0:
        x = x + (stack.pop)[^1]
      result += x

  part 2:
    result = 0
    for nums in numberLines:
      var stack = @[nums]

      block buildup:
        var keepGoing = true
        while keepGoing:
          let line = stack[^1]
          var diff: seq[int]
          keepGoing = false
          for i in 1..line.high:
            diff.add(line[i] - line[i - 1])
            keepGoing = true
          if keepGoing:
            stack.add diff

      var x = 0
      while stack.len > 0:
        x = (stack.pop)[0] - x
      echo x
      result += x

  # 2658601764 is too high
  verifyPart(1, 1921197370)
  # 20942 is too high
  verifyPart(2, 1124)
