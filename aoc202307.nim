import aocd
import std/[sugar, tables, algorithm, strformat, strutils, strscans, sequtils, re, unittest]

const example1 = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"""

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
  of 'J': '*'
  else: c.tr1

type
  Hand = tuple[cards: array[5, char], bid: int]

func `$`(h: Hand): string =
  fmt"{h.cards.join} {h.bid}"

proc toHand(cards: string, bid: int = 1): Hand =
  let (ok, c1, c2, c3, c4, c5) = cards.scanTuple("$c$c$c$c$c$.")
  doAssert ok
  return (cards: [c1, c2, c3, c4, c5], bid: bid)

proc countCards(cards: array[5, char]): tuple[jokers: int, counts: seq[int]] =
  var table = initTable[char, int]()
  for c in cards:
    table[c] = table.getOrDefault(c, 0) + 1
  let jokers = table.getOrDefault('*', 0)
  if '*' in table:
    table.del('*')
  result = (jokers: jokers, counts: table.values.toSeq)

const
  FiveOfAKind = 7
  FourOfAKind = 6
  FullHouse = 5
  ThreeOfAKind = 4
  TwoPairs = 3
  OnePair = 2
  HighCard = 1

proc score(h: Hand): int =
  let (jokers, counts) = countCards(h.cards)

  # Five of a kind
  if jokers == 5 or counts.anyIt(it >= 5 - jokers):
    return FiveOfAKind
  # Four of a kind
  if counts.anyIt(it >= 4 - jokers):
    return FourOfAKind
  # Full house, 3 + 2
  if 3 in counts and 2 in counts:
    return FullHouse
  if jokers == 1 and 3 in counts:
    return FullHouse
  if jokers == 1 and counts.countIt(it == 2) == 2:
    return FullHouse
  if jokers == 2 and 2 in counts:
    return FullHouse
  # Three of a kind, 3
  if counts.anyIt(it >= 3 - jokers):
    return ThreeOfAKind
  # Two pairs, 2 + 2
  if counts.countIt(it == 2) == 2:
    return TwoPairs
  if jokers >= 1 and 2 in counts:
    return TwoPairs
  # One pair, 2
  if jokers >= 1 or 2 in counts:
    return OnePair
  # High card
  return HighCard

type
  Tr = proc(c: char): char
  Cmp = proc(a: Hand, b: Hand): int

proc cmpF(tr: Tr): Cmp =
  proc cmp(a: Hand, b: Hand): int =
    if (let (sa, sb) = (score(a), score(b)); sa != sb):
      return cmp(sa, sb)
    for i in 0..4:
      let ac = tr(a.cards[i])
      let bc = tr(b.cards[i])
      if ac != bc:
        return cmp(ac, bc)
    return 0
  return cmp

let
  cmp1 = cmpF(tr1)
  cmp2 = cmpF(tr2)

day 7:
  let hands = input.strip.splitLines.map do (line: string) -> Hand:
    let (ok, c1, c2, c3, c4, c5, bid) = line.scanTuple("$c$c$c$c$c $i$.")
    doAssert ok
    (cards: [c1, c2, c3, c4, c5], bid: bid)

  part 1:
    result = 0
    for rank, hand in hands.sorted(cmp1):
      result += (rank + 1) * hand.bid

  part 2:
    result = 0
    for rank, hand in hands.sorted(cmp2):
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

  check cmp2(fiveOfAKind, fullHouse) == 1
  check cmp2(fullHouse, twoPairs) == 1
  check cmp2(twoPairs, fiveOfAKind) == -1
  check cmp2(twoPairs, fiveHigh) == 1
  check cmp2(kingHigh, tenHigh) == 1
  check cmp2(tenHigh, kingHigh) == -1
  check cmp2(fiveHigh, fourHigh) == 1
  check cmp2(fourHigh, fiveHigh) == -1
  check cmp2(fiveHigh, fiveHigh) == 0
  check cmp2(threeOfAKind, (cards: ['2', '3', 'J', 'K', '3'], bid: 2)) == 1
