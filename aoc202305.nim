import aocd
import unicode except strip
import std/[algorithm, deques, random, strscans, sugar, strutils, strformat, sequtils, re, intsets, tables, unittest]

const example1 = """
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"""

type
  Range = tuple[low: int, high: int]
  Target = tuple[category: string, value: int]
  Offsets = seq[seq[tuple[low: int, high: int, offset: int]]]

day 5:
  let i = example1.strip.splitLines
  let seeds = i[0].substr(7).split(" ").map(parseInt)
  var offsets: Offsets = @[]

  # I checked and non of the mappings overlap.
  for i, group in i[1..^1].groupByBlank.pairs:
    offsets.add @[]
    for line in group[1..^1]:
      let (ok, dest, source, length) = line.scanTuple("$i $i $i$.")
      offsets[i].add((low: source, high:source+length-1, offset: dest - source))

  part 1:
    var lowest = int.high

    for seed in seeds:
      var value = seed
      for i, group in offsets:
        let t = group.filterIt(value in it.low..it.high)
        if t.len > 0:
          value += t[0].offset
      lowest = min(lowest, value)

    lowest

  part 2:
    var frontier = initDeque[tuple[low: int, high: int, r: int]]()

    for seed in seeds.distribute(seeds.len div 2, false):
      let start = seed[0]
      let length = seed[1]
      frontier.addLast((low: start, high: start + length - 1, r: 0))

    while frontier.len > 0:
      var c = frontier.popFirst

      if c.r >= offsets.len:
        frontier.addFirst(c)
        break

      # "consume" parts of the candidate that matched a mapping,
      # and if there is anything left after going through all mappings,
      # pass that part through without offset
      var consumed: seq[Range] = @[]
      for mapping in offsets[c.r]:
        let ol = max(c.low, mapping.low)
        let oh = min(c.high, mapping.high)
        if ol <= oh:
          consumed.add((low: mapping.low, high: mapping.high))
          frontier.addLast((low: ol+mapping.offset, high: oh+mapping.offset, r: c.r+1))

      # Pass through parts of the candidate that did not match any mapping, if
      # any remain.
      if consumed.len > 0:
        var x = c.low
        var i = c.low
        var j = c.low
        var burned = false
        while x in c.low..c.high:
          burned = false
          # check if x is consumed
          for (low, high) in consumed:
            if x in low..high:
              burned = true
              x = high
              break

          if not burned:
            x = consumed.mapIt(it.low).min

          if burned and i < j:
            frontier.addLast((low: i, high: j, r: c.r+1))
          if burned:
            i = x+1
            j = x+1
          else:
            j = x

          x.inc

      else:
        frontier.addLast((low: c.low, high: c.high, r: c.r+1))

    frontier.mapIt(it.low).min

  verifyPart(1, 388071289)
  verifyPart(2, 84206669)
