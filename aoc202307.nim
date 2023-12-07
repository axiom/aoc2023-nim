import aocd
import std/[tables, algorithm, strformat, strutils, strscans, sequtils, re, unittest]

const order = """AKQJT98765432*"""

func tr1(c: char): char =
  case c
  of 'T': 'A'
  of 'J': 'B'
  of 'Q': 'C'
  of 'K': 'D'
  of 'A': 'E'
  else: c

func tr2(c: char): char =
  case c
  of 'T': 'A'
  of 'J': '*'
  of 'Q': 'C'
  of 'K': 'D'
  of 'A': 'E'
  else: c

var tr = tr1

const example1 = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"""

type
  Hand = tuple[cards: array[5, char], bid: int]
const
  FiveOfAKind = 7
  FourOfAKind = 6
  FullHouse = 5
  ThreeOfAKind = 4
  TwoPairs = 3
  OnePair = 2
  HighCard = 1

func `$`(h: Hand): string =
  let (cards, bid) = h
  return fmt"{cards.join} {bid}"

proc toHand(cards: string): Hand =
  let (ok, c1, c2, c3, c4, c5) = cards.scanTuple("$c$c$c$c$c$.")
  doAssert ok
  return (cards: [c1, c2, c3, c4, c5], bid: 0)

proc score(h: Hand): int =
  # Can no longer trust sorted due to the wildcard...
  var counts: Table[char, int] = initTable[char, int]()
  for c in h.cards:
    counts[c.tr] = 0
  for c in h.cards:
    counts[c.tr] += 1
  let jokers = counts.getOrDefault('*', 0)
  counts.del('*')
  let cardCounts = counts.values.toSeq

  # Five of a kind
  if jokers == 5 or cardCounts.anyIt(it >= 5 - jokers):
    return FiveOfAKind
  # Four of a kind
  if cardCounts.anyIt(it >= 4 - jokers):
    return FourOfAKind
  # Full house, 3 + 2
  if 3 in cardCounts and 2 in cardCounts:
    return FullHouse
  if jokers == 1 and 3 in cardCounts:
    return FullHouse
  if jokers == 1 and cardCounts.countIt(it == 2) == 2:
    return FullHouse
  if jokers == 2 and 2 in cardCounts:
    return FullHouse
  # Three of a kind, 3
  if cardCounts.anyIt(it >= 3 - jokers):
    return ThreeOfAKind
  # Two pairs, 2 + 2
  if cardCounts.countIt(it == 2) == 2:
    return TwoPairs
  if jokers >= 1 and 2 in cardCounts:
    return TwoPairs
  # One pair, 2
  if jokers >= 1 or 2 in cardCounts:
    return OnePair
  # High card
  return HighCard

proc cmp(a: Hand, b: Hand): int =
  if (let (sa, sb) = (score(a), score(b)); sa != sb):
    return cmp(sa, sb)
  for i in 0..4:
    let ac = a.cards[i].tr
    let bc = b.cards[i].tr
    if ac != bc:
      return cmp(ac, bc)
  return 0

day 7:
  let hands = input.strip.splitLines.map do (line: string) -> Hand:
    let (ok, c1, c2, c3, c4, c5, bid) = line.scanTuple("$c$c$c$c$c $i$.")
    doAssert ok
    (cards: [c1, c2, c3, c4, c5], bid: bid)

  part 1:
    tr = tr1
    result = 0
    for rank, hand in hands.sorted(cmp):
      result += (rank + 1) * hand.bid

  part 2:
    tr = tr2
    result = 0
    for rank, hand in hands.sorted(cmp):
      result += (rank + 1) * hand.bid

  # 247090697 is too high
  verifyPart(1, 245794640)
  # 247282879, 245749785 is too low
  # 247917290 is too high
  verifyPart(2, 247899149)

block unittests:
  let fiveOfAKind = (cards: ['A', 'A', 'A', 'A', 'A'], bid: 1)
  let fullHouse = (cards: ['A', 'K', 'A', 'K', 'A'], bid: 1)
  let threeOfAKind = (cards: ['2', '3', '8', '8', '8'], bid: 1)
  let twoPairs = (cards: ['A', 'K', 'A', 'K', 'Q'], bid: 1)
  let kingHigh = (cards: ['K', 'Q', 'J', 'T', '9'], bid: 1)
  let tenHigh = (cards: ['T', '9', 'A', '7', '6'], bid: 1)

  tr = tr2

  check score(fiveOfAKind) == 7
  check score((cards: ['A', 'A', 'A', 'A', '*'], bid: 1)) == 7
  check score((cards: ['*', 'A', 'A', 'A', '*'], bid: 1)) == 7
  check score((cards: ['*', 'A', '*', 'A', '*'], bid: 1)) == 7
  check score((cards: ['*', '*', '*', 'A', '*'], bid: 1)) == 7
  check score((cards: ['*', '*', '*', '*', '*'], bid: 1)) == 7
  check score(fullHouse) == 5
  check score((cards: ['*', 'K', 'A', 'K', 'A'], bid: 1)) == 5
  check score((cards: ['*', '*', 'A', 'K', 'A'], bid: 1)) == 5
  check score(twoPairs) == 3
  check score(threeOfAKind) == 4

  check score("KK677".toHand) == 3

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
  check cmp(threeOfAKind, (cards: ['2', '3', 'J', 'K', '3'], bid: 2)) == 1
