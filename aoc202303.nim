import aocd
import std/[strutils, intsets, tables, unittest]

func isSymbol(c: char): bool =
  not (c in Digits) and c != '.'

func isGear(c: char): bool =
  c == '*'

type rowcol = int

func encode(row: int, col: int): rowcol =
  return rowcol(row shl 16 or col)

func row(pos: rowcol): int = int(pos) shr 16
func col(pos: rowcol): int = int(pos) and 0xffff

iterator neighbors(pos: rowcol): rowcol =
  for dx in -1..1:
    for dy in -1..1:
      if dx != 0 or dy != 0:
        yield encode(row(pos) + dy, col(pos) + dx)

proc adjacent(symbols: IntSet, start: rowcol, number: int): bool =
  let
    x = col(start)
    y = row(start)
  for o in 0..high($number):
    for pos in neighbors(encode(y, x + o)):
      if pos in symbols:
        return true

day 3:
  var
    symbols = initIntSet()
    gears = initIntSet()
    partPos = initTable[rowcol, int]()
    partSum = 0

  # Find all the symbols and gears
  for y, row in lines:
    for x, c in row:
      if c.isSymbol:
        symbols.incl(encode(y, x))
      if c.isGear:
        gears.incl(encode(y, x))

  # Find all parts numbers adjacent to symbols
  for y, line in lines:
    var inDigits, wasInDigits = false
    var s, e = 0

    for i in 0..line.len:
      inDigits = if i < line.len: line[i] in Digits else: false

      if wasInDigits and inDigits:
        e = i
      elif not wasInDigits and inDigits:
        s = i
        e = i
      elif wasInDigits and not inDigits:
        let partNumber = line.substr(s, e).parseInt

        if symbols.adjacent(encode(y, s), partNumber):
          partSum += partNumber

          # Record where the part numbers are
          for x in s..e:
            partPos[encode(y, x)] = partNumber

        s = i
        e = i

      wasInDigits = inDigits

  part 1:
    partSum

  part 2:
    var gearRatioSum = 0
    for gear in gears:
      var gearRatio = 1
      var gearParts = initIntSet()
      for pos in neighbors(gear):
        if pos in partPos:
          gearParts.incl(partPos[pos])

      if gearParts.len == 2:
        for part in gearParts:
          gearRatio *= part
        gearRatioSum += gearRatio
    gearRatioSum

  verifyPart(1, 546563)
  verifyPart(2, 91031374)
