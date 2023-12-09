import aocd
import std/[deques, strformat, strscans, strutils, sequtils, unittest]

const example1 {.used.} = """
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
  Mapping = tuple[low: int, high: int, offset: int]

day 5:
  # I checked and non of the mappings overlap.

  var mappings: seq[seq[Mapping]] = @[]

  # First line is special, it contains the seeds.
  let seeds = inputLines[0].substr(7).split(" ").map(parseInt)

  # The rest of the lines are grouped by blank lines, and contain the
  # transformations.
  for i, group in inputLines[1..^1].groupByBlank.pairs:
    mappings.add @[]
    for line in group[1..^1]:
      let (ok, dst, src, len) = line.scanTuple("$i $i $i$.")
      doAssert ok, fmt"Invalid mapping: {line}"
      mappings[i].add((low: src, high: src+len-1, offset: dst-src))

  part 1:
    result = int.high
    for seed in seeds:
      var location = seed
      for i, transformations in mappings:
        for trans in transformations.filterIt(location in it.low..it.high):
          location += trans.offset
      result = min(result, location)

  part 2:
    var frontier = initDeque[tuple[low: int, high: int, r: int]]()

    for seed in seeds.distribute(seeds.len div 2, false):
      frontier.addLast((low: seed[0], high: seed[0] + seed[1] - 1, r: 0))

    while frontier.len > 0:
      var c = frontier.popFirst

      # Check if we are done, by having reached the location.
      if c.r >= mappings.len:
        frontier.addFirst(c)
        break

      # Keep track of the parts of the candidate that are not mapped yet.
      var slops: seq[Range] = @[(low: c.low, high: c.high)]

      for mapping in mappings[c.r]:
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
        mappings[c.r].allIt do:
          let ol = max(slop.low, it.low)
          let oh = min(slop.high, it.high)
          ol > oh

      # Pass through any part of the candidate that did not get transformed.
      for slop in slops:
        frontier.addLast((low: slop.low, high: slop.high, r: c.r+1))

    # Now just pick the lowest location value.
    doAssert frontier.len != 0
    frontier.mapIt(it.low).min

  verifyPart(1, 388071289)
  verifyPart(2, 84206669)
