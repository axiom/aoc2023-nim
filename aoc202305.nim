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

func `*`(a: Range, b: Range): Range =
  let ol = max(a.low, b.low)
  let oh = min(a.high, b.high)
  if ol <= oh:
    (low: ol, high: oh)
  else:
    (low: 0, high: -1)

func `+`(a: Range, o: int): Range =
  (low: a.low + o, high: a.high + o)

func isEmpty(a: Range): bool =
  a.low > a.high

day 5:
  let i = input.strip.splitLines
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
      frontier.addLast((low: seed[0], high: seed[0] + seed[1] - 1, r: 0))

    while frontier.len > 0:
      var c = frontier.popFirst

      # Check if we are done
      if c.r >= offsets.len:
        frontier.addFirst(c)
        break

      # Keep track of the parts of the candidate that are not mapped yet.
      var slops: seq[Range] = @[(low: c.low, high: c.high)]

      for mapping in offsets[c.r]:
        let ol = max(c.low, mapping.low)
        let oh = min(c.high, mapping.high)

        # If the candidate range intersects with this mapping, transform it.
        if ol <= oh:
          frontier.addLast((low: ol+mapping.offset, high: oh+mapping.offset, r: c.r+1))

          # Maybe pass through the parts of the candidate that did not intersect
          # with the mapping.
          if c.low < ol:
            slops.add((low: c.low, high: ol-1))
          if oh < c.high:
            slops.add((low: oh+1, high: c.high))

      # Filter out slops that overlap with some mapping.
      slops = slops.filterIt do:
        let slop = it
        offsets[c.r].allIt do:
          let ol = max(slop.low, it.low)
          let oh = min(slop.high, it.high)
          ol > oh

      # Nothing mapped, pass value through as is.
      for slop in slops:
        frontier.addLast((low: slop.low, high: slop.high, r: c.r+1))

    doAssert frontier.len != 0
    frontier.mapIt(it.low).min

  verifyPart(1, 388071289)
  verifyPart(2, 84206669)
