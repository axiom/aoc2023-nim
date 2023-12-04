import aocd
import unicode except strip
import std/[deques, strutils, sequtils, re, packedsets], unittest

const example1 = """
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"""

type Card = tuple[id: int, winning: PackedSet[uint8], drawn: PackedSet[uint8]]

func extractNumbers(input: string): seq[uint8] =
  input.strip.filterIt(it.isDigit or it == ' ').join.strip.split(re" +").mapIt(uint8(it.parseInt))

func extractCards(input: string): seq[Card] =
  for x in input.strip.splitLines.mapIt(
    it
      .split(":|".toRunes)
      .mapIt(it.extractNumbers)):
        result.add((id: int(x[0][0]), winning: x[1].toPackedSet, drawn: x[2].toPackedSet))

day(2023, 4):
  let cards = input.extractCards

  part(1):
    var points = 0

    for card in cards:
      var score = 0
      for number in card.drawn:
        if number in card.winning:
          if score == 0:
            score = 1
          else:
            score *= 2
      points += score

    points

  # not 1178, 1913, 1043
  check(1, 23678)

  part(2):
    var scratchcards = cards.toDeque
    var copies = cards.mapIt(1)

    for i, (_, winning, drawn) in cards.pairs:
      var matching = 0
      for number in drawn:
        if number in winning:
          matching += 1

      for m in 1..matching:
        if i + m < copies.len:
          copies[i + m] += copies[i]

    copies.foldl(a + b)

  check(2, 15455663)
