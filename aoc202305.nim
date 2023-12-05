import aocd
import unicode except strip
import std/[strscans, sugar, strutils, strformat, sequtils, re, intsets, tables, unittest]

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
  Range = tuple[source: int, dest: int, length: int]
  Target = tuple[category: string, value: int]

day 5:
  let i = input.strip.splitLines
  let seeds = i[0].substr(7).split(" ").map(parseInt)
  var transformer = newTable[string, proc (source: int): Target]()

  for x in i[1..^1].groupByBlank:
    let (ok, source, destination) = x[0].scanTuple("$w-to-$w map:")
    var ranges = newSeq[Range]()
    for line in x[1..^1]:
      let (ok, destStart, sourceStart, length) = line.scanTuple("$i $i $i$.")
      ranges.add((source: sourceStart, dest: destStart, length: length))

    capture source, destination, ranges:
      transformer[source] = proc (source: int): Target =
        result = (category: destination, value: source)
        for (sourceStart, destStart, length) in ranges:
          if source in sourceStart..sourceStart+length-1:
            result = (category: destination, value: destStart + (source - sourceStart))

  part 1:
    var lowest = int.high

    for seed in seeds:
      var value: int = seed
      var category: string = "seed"
      while category != "location":
        let target = (transformer[category])(value)
        category = target.category
        value = target.value
      lowest = min(lowest, value)

    lowest

  part 2:
    # I think we need to go for dynamic programming

    var lowest = int.high

    for i in seeds.low..seeds.high:
      if i mod 2 == 1: continue
      echo fmt"i = {i}, {seeds[i]}..{seeds[i] + seeds[i+1]-1}"
      for seed in seeds[i]..seeds[i]+seeds[i+1]-1:
        var value: int = seed
        var category: string = "seed"
        while category != "location":
          let target = (transformer[category])(value)
          category = target.category
          value = target.value
        lowest = min(lowest, value)

    lowest

  # verifyPart(1, 388071289)
