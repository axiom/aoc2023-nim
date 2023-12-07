import aocd
import unicode except strip
import std/[algorithm, strformat, strutils, strscans, sequtils, re, intsets, unittest]

const order = """AKQJT98765432"""

func tr(c: char): char =
  case c
  of 'T': 'A'
  of 'J': 'B'
  of 'Q': 'C'
  of 'K': 'D'
  of 'A': 'E'
  else: c

func rt(c: char): char =
  case c
  of 'A': 'T'
  of 'B': 'J'
  of 'C': 'Q'
  of 'D': 'K'
  of 'E': 'A'
  else: c

const example1 = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"""

const order1 = """
32T3K 765
KTJJT 220
KK677 28
T55J5 684
QQQJA 483
"""

type
  Hand = tuple[cards: array[5, char], bid: int]

func `$`(h: Hand): string =
  let (cards, bid) = h
  return fmt"{cards.map(rt).join} {bid}"

proc score(h: Hand): int =
  let cards = h.cards.sorted
  let (a, b, c, d, e) = (cards[0], cards[1], cards[2], cards[3], cards[4])
  # Five of a kind
  if a == b and b == c and c == d and d == e:  return 7
  # Four of a kind
  if (a == b or d == e) and b == c and c == d:  return 6
  # Full house, 3 + 2
  if a == b and b == c and d == e:  return 5
  if a == b and c == d and c == e:  return 5
  # Three of a kind, 3
  if a == b and b == c:  return 4
  if b == c and c == d:  return 4
  if c == d and d == e:  return 4
  # Two pairs, 2 + 2
  if a == b and c == d:  return 3
  if a == b and d == e:  return 3
  if b == c and d == e:  return 3
  # One pair, 2
  if a == b:  return 2
  if b == c:  return 2
  if c == d:  return 2
  if d == e:  return 2
  # High card
  return 0

proc cmp(a: Hand, b: Hand): int =
  if (let (sa, sb) = (score(a), score(b)); sa != sb):
    return cmp(sa, sb)
  for i in 0..4:
    if a.cards[i] != b.cards[i]:
      return cmp((a.cards[i]), (b.cards[i]))
  return 0

day 7:
  part 1:
    let hands = input.strip.splitLines.map do (line: string) -> Hand:
      let (ok, c1, c2, c3, c4, c5, bid) = line.scanTuple("$c$c$c$c$c $i$.")
      doAssert ok
      (cards: [c1.tr, c2.tr, c3.tr, c4.tr, c5.tr], bid: bid)

    result = 0
    for rank, hand in hands.sorted(cmp):
      echo fmt"{rank + 1}: {hand}"
      result += (rank + 1) * hand.bid

  part 2:
    42

  # 247090697 is too high
  verifyPart(1, 6440)
  verifyPart(2, 42)

block unittests:
  let fiveOfAKind = (cards: ['A'.tr, 'A'.tr, 'A'.tr, 'A'.tr, 'A'.tr], bid: 1)
  let fullHouse = (cards: ['A'.tr, 'K'.tr, 'A'.tr, 'K'.tr, 'A'.tr], bid: 1)
  let threeOfAKind = (cards: ['2', '3', '8', '8', '8'], bid: 1)
  let twoPairs = (cards: ['A'.tr, 'K'.tr, 'A'.tr, 'K'.tr, 'Q'.tr], bid: 1)
  let kingHigh = (cards: ['K'.tr, 'Q'.tr, 'J'.tr, 'T'.tr, '9'], bid: 1)
  let tenHigh = (cards: ['T'.tr, '9', 'A'.tr, '7', '6'], bid: 1)

  check score(fiveOfAKind) == 7
  check score(fullHouse) == 5
  check score(twoPairs) == 3
  check score(threeOfAKind) == 4

  let fiveHigh = (cards: ['5', '4', '3', '2', '2'], bid: 1)
  let fourHigh = (cards: ['4', '5', '3', '2', '2'], bid: 1)

  check cmp(fiveOfAKind, fullHouse) == 1
  check cmp(fullHouse, twoPairs) == 1
  check cmp(twoPairs, fiveOfAKind) == -1
  check cmp(twoPairs, fiveHigh) == 1
  check cmp(kingHigh, tenHigh) == 1
  check cmp(tenHigh, kingHigh) == -1
  check cmp(fiveHigh, fourHigh) == 1
  check cmp(fourHigh, fiveHigh) == -1
  check cmp(fiveHigh, fiveHigh) == 0
  check cmp(threeOfAKind, (cards: ['2', '3', 'J'.tr, 'K'.tr, '3'], bid: 2)) == 1
