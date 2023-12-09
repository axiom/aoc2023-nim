import aocd
import unicode except strip
import std/[strutils, sequtils, strscans, unittest]

day 2:
  var possibleGameSum, powerSetSum = 0

  for game in inputLines:
    var red, green, blue = 1
    for color in game.split(",;:".toRunes):
      let (_, balls, color) = color.strip.scanTuple("$i $w$.")
      case color:
        of "red": red = max(red, balls)
        of "green": green = max(green, balls)
        of "blue": blue = max(blue, balls)
        else: discard

    if red <= 12 and green <= 13 and blue <= 14:
      let (_, id) = game.scanTuple("Game $i:")
      possibleGameSum += id

    powerSetSum += red * green * blue

  part 1:
    possibleGameSum

  part 2:
    powerSetSum

  verifyPart(1, 2207)
  verifyPart(2, 62241)
