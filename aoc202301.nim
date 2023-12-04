import aocd
import std/[strutils, sequtils], unittest

const digitNames = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

func extractDigits(str: string): seq[int] =
  template maybeAdd(digit: int) =
    if (digit in 0..9):
      result.add(digit)
      continue

  for (i, c) in str.pairs:
    maybeAdd(c.ord - '0'.ord)
    maybeAdd(digitNames.find(str.substr(i, i + 2)))
    maybeAdd(digitNames.find(str.substr(i, i + 3)))
    maybeAdd(digitNames.find(str.substr(i, i + 4)))

check extractDigits("123") == @[1, 2, 3]
check extractDigits("one two three") == @[1, 2, 3]
check extractDigits("twone") == @[2, 1]

func combine(digits: seq[int]): int =
  10 * digits[0] + digits[digits.high]

check combine(@[1]) == 11
check combine(@[1, 2]) == 12
check combine(@[1, 2, 3]) == 13

day(2023, 1):
  let lines = input.strip.splitLines

  part(1):
    lines
      .mapIt(it.filter(isDigit).mapIt(parseInt($it)))
      .map(combine)
      .foldl(a + b)

  check(1, 54990)

  part(2):
    lines
      .map(extractDigits)
      .map(combine)
      .foldl(a + b)

  check(2, 54473)
