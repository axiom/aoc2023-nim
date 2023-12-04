import aocd
import unicode except strip
import std/[strutils, sequtils, strscans, strformat], unittest

day(2023, 2):
  let lines = input.strip.splitLines
  var possibleGameSum, powerSetSum = 0

  for game in lines:
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

  part(1):
    possibleGameSum

  part(2):
    powerSetSum

  check(1, 2207)
  check(2, 62241)
