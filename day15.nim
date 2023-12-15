import aocd
import std/[strutils, sequtils, strformat, tables, unittest]

const example {.used.} = """
"""

func hash (input: string): int =
  result = 0
  for c in input:
    result += c.ord
    result *= 17
    result = result mod 256

check "HASH".hash == 52

day 15:
  let sequences = inputLines.mapIt(it.split(","))

  part 1:
    result = 0
    for sequence in sequences:
      for step in sequence:
        result += step.hash

  part 2:
    3

  verifyPart(1, 515210)
  # verifyPart(2, 0)
