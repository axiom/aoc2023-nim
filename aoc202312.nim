import aocd
import std/[tables, heapqueue, strutils, strformat, sets, sequtils, unittest]

const example1 {.used.} = """
???.###              1,1,3
.??..??...?##.       1,1,3
?#?#?#?#?#?#?#?      1,3,1,6
????.#...#...        4,1,1
????.######..#####.  1,6,5
?###????????         3,2,1
"""

type
  Nucleotide = enum
    Operational, Damaged, Unknown
  Sequence = seq[Nucleotide]
  Record = object
    genome: Sequence
    sequences: seq[Sequence]


# Print data structures
func `$`(n: Nucleotide): string =
  case n
  of Operational: "."
  of Damaged: "#"
  of Unknown: "?"

func `$`(s: Sequence): string =
  result = s.mapIt($it).join


# Parse input and build up data structures
func damagedSequence(run: int): Sequence =
  result = newSeqWith[Nucleotide](run + 2, Damaged)
  result[0] = Operational
  result[result.high] = Operational

const CharToNucleotide = {
  '.': Operational,
  '#': Damaged,
  '?': Unknown,
}.toTable

func toSequence(s: string): Sequence =
  result.add Operational
  result = result.concat s.mapIt(CharToNucleotide[it])
  result.add Operational


# Solve alignment problem
type Key = object
  dmg, pos: int

func damage(sequence: Sequence): int =
  sequence.len - 2

proc fits(genome: Sequence, sequence: Sequence): bool =
  for i in 0..sequence.high:
    if genome[i] == Unknown:
      continue
    if genome[i] != sequence[i]:
      return false
  true

type SolutionTable = seq[seq[int]]

proc buildTable(genome: Sequence, sequences: seq[Sequence]): SolutionTable =
  result = newSeqWith[seq[int]](sequences.len, newSeqWith[int](genome.len, 0))

  # var seen: HashSet[Key]
  # Build up table row for row.
  for y in 0..sequences.high:
    let sequence = sequences[y]
    for x in 0..genome.len - sequences[y].len:
      let genomPart = genome[x..x + sequence.len - 1]
      if genomPart.fits sequence:
        result[y][x] = x + sequence.len - 1

  # Clear impossible from from the next row
  for y in 0..result.high-1:
    var minPos = result[y].max
    for x in 0..result[y].high:
      if result[y][x] == 0:
        continue
      minPos = min(minPos, result[y][x])
    for x in 0..<minPos:
      for yy in y+1..result.high:
        result[yy][x] = 0

type StackKey = object
  i: int
  minPos: int
  path: seq[int]

func `<`(a, b: StackKey): bool =
  a.i < b.i

proc countSolutions(table: SolutionTable, sequences: seq[Sequence], lessThan: int): int =
  # echo ""
  # for y in 0..table.high:
  #   if y == 0:
  #     for x in 0..table[y].high:
  #       stdout.write fmt"{x:-2} "
  #     echo ""
  #   for x in 0..table[y].high:
  #     stdout.write fmt"{table[y][x]:-2} "
  #   echo ""

  if table.len == 0 or sequences.len == 0:
    return 1

  result = 0
  for y in countdown(table.high, table.low):
    for x in 0..lessThan:
      if table[y][x] > 0:
        result += countSolutions(table[0..^2], sequences[0..^1], x)

  # var paths = initHeapQueue[StackKey]()
  #
  # for x in 0..table[0].high:
  #   let minPos = table[0][x]
  #   if minPos > 0:
  #     paths.push(StackKey(i: 1, minPos: minPos, path: @[x]))
  #
  # for y in 1..table.high:
  #   var newPaths = initHeapQueue[StackKey]()
  #   while paths.len > 0:
  #     let path = paths.pop
  #
  #     for x in path.minPos..table[y].high:
  #       let minPos = table[y][x]
  #       if minPos > 0:
  #         let nextPath = StackKey(i: path.i + 1, minPos: minPos, path: path.path & @[x])
  #         # echo nextPath
  #         newPaths.push(nextPath)
  #
  #   paths = newPaths
  #
  # while paths.len > 0:
  #   let path = paths.pop
  #   result += 1
  #   echo path.path

proc solve(genome: Sequence, sequences: seq[Sequence]): int =
  buildTable(genome, sequences).countSolutions(sequences, genome.high)

# Check the examples provided
check solve(toSequence("?"), @[1].map(damagedSequence)) == 1
check solve(toSequence("??"), @[1].map(damagedSequence)) == 2
check solve(toSequence("?.?"), @[1].map(damagedSequence)) == 2
check solve(toSequence("?.?"), @[1, 1].map(damagedSequence)) == 1
check solve(toSequence("..."), @[1, 1].map(damagedSequence)) == 0
check solve(toSequence("..."), @[1].map(damagedSequence)) == 0
check solve(toSequence("###"), @[1].map(damagedSequence)) == 0
check solve(toSequence("###"), @[3].map(damagedSequence)) == 1
check solve(toSequence("###"), @[4].map(damagedSequence)) == 0
check solve(toSequence(".............."), @[1,1,3].map(damagedSequence)) == 0
check solve(toSequence(".??..??...?##."), @[1,1,3].map(damagedSequence)) == 4
check solve(toSequence("???.###"), @[1,1,3].map(damagedSequence)) == 1
check solve(toSequence("?#?#?#?#?#?#?#?"), @[1,3,1,6].map(damagedSequence)) == 1
check solve(toSequence("????.#...#..."), @[4,1,1].map(damagedSequence)) == 1
check solve(toSequence("????.######..#####."), @[1,6,5].map(damagedSequence)) == 4
check solve(toSequence("?###????????"), @[3,2,1].map(damagedSequence)) == 10
check solve(toSequence("???.###"), @[1,1,3].map(damagedSequence)) == 1


func unfold(genome: string, sep: string): string =
  [genome, genome, genome, genome, genome].join sep

check unfold(".#", "?") == ".#?.#?.#?.#?.#"

day 12:
  var springRecords: seq[Record]
  var unfoldedRecords: seq[Record]

  for line in example1.strip.splitLines:
    let parts = line.splitWhitespace

    # Original sequences
    block:
      let genome = parts[0]
      let sequences = parts[1].split(",").mapIt(it.parseInt.damagedSequence)
      springRecords.add(Record(genome: genome.toSequence, sequences: sequences))

    # Unfold the sequenes
    block:
      let genome = parts[0].unfold("?")
      let sequences = parts[1].unfold(",").split(",").mapIt(it.parseInt.damagedSequence)
      unfoldedRecords.add(Record(genome: genome.toSequence, sequences: sequences))

  part 1:
    # return 7460
    result = 0
    for org in springRecords:
      let solutions = solve(org.genome, org.sequences)
      # echo fmt"{org.genome} {org.sequences} -> {solutions}"
      result += solutions

  part 2:
    # return 525152
    result = 0
    for org in unfoldedRecords:
      let solutions = solve(org.genome, org.sequences)
      # echo fmt"{org.genome} {org.sequences} -> {solutions}"
      result += solutions

  verifyPart(1, 7460)
  verifyPart(2, 525152)
