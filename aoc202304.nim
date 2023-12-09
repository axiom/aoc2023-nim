import aocd
import unicode except strip
import std/[strutils, sequtils, re, intsets, unittest]

const example1 {.used.} = """
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"""

func extractNumbers(input: string): seq[int] =
  input.strip.filterIt(it.isDigit or it == ' ').join.strip.split(re" +").map(parseInt)

day 4:
  let cards = inputLines
    .mapIt(it.split(":|".toRunes).mapIt(it.extractNumbers))
    .mapIt((winning: it[1].toIntSet, drawn: it[2].toIntSet))

  part 1:
    result = 0
    for card in cards:
      if (let score = 1 shl len(card.winning * card.drawn); score > 1):
        result += score shr 1

  part 2:
    var copies = cards.mapIt(1)

    for i, (winning, drawn) in cards.pairs:
      for o in 1..len(winning * drawn):
        if (let j = i + o; j < copies.len):
          copies[j] += copies[i]

    copies.foldl(a + b)

  verifyPart(1, 23678)
  verifyPart(2, 15455663)
