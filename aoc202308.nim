import aocd
import std/[strutils, math, strscans, tables, intsets, sequtils, unittest]

const example1 = """
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"""

const example2 = """
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"""

const example3 = """
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
"""

type
  Address = int
  Node = tuple[left: int, right: int]

func encode(str: string): Address =
  for c in str:
    let v = c.ord - '0'.ord
    result = (result shl 8) or v

func decode(address: Address): char =
  let x: range[0..255] = address and 0b11111111
  result = chr(x + '0'.ord)

func fullDecode(address: Address): string =
  let a: range[0..255] = (address shr 0) and 0b11111111
  let b: range[0..255] = (address shr 8) and 0b11111111
  let c: range[0..255] = (address shr 16) and 0b11111111
  [c, b, a].mapIt(chr(it + '0'.ord)).join

check "AAA".encode.decode == 'A'
check "AAZ".encode.decode == 'Z'
check "12X".encode.decode == 'X'
check "AAA".encode.fullDecode == "AAA"
check "AAZ".encode.fullDecode == "AAZ"
check "12X".encode.fullDecode == "12X"
check "ZZZ".encode.fullDecode == "ZZZ"

template finished(cursors: seq[Address]): bool =
  cursors.allIt((it and 0xff) == 42)

func isEnd(address: Address): bool =
  (address and 0xff) == 42

check isEnd("AAA".encode) == false
check isEnd("ZZZ".encode) == true

func isStart(address: Address): bool =
  (address and 0xff) == 17

check isStart("AAA".encode) == true
check isStart("ZZZ".encode) == false

day 8:
  let xlines = input.strip.splitLines
  let instructions = xlines[0]
  var cursors: seq[Address] = @[]
  var starts: seq[Address] = @[]
  var graph = initTable[Address, Node]()
  for line in xlines[2..^1]:
    let (ok, a, l, r) = line.scanTuple("$+ = ($+, $+)$.")
    doAssert ok
    graph[encode(a)] = (left: encode(l), right: encode(r))
    if a[2] == 'A':
      cursors.add encode(a)
      starts.add encode(a)

  part 1:
    let start = "AAA".encode
    let target = "ZZZ".encode

    var steps = 0
    var ip = 0
    var cursor = start
    while cursor != target:
      steps += 1
      case instructions[ip]:
        of 'L':
          cursor = graph[cursor].left
        of 'R':
          cursor = graph[cursor].right
        else:
          doAssert false
      ip = (ip + 1) mod instructions.len
    steps

  part 2:
    # Having looked at a graph of the data, it is clear that we have a "circle"
    # graph for each entry node. So thea idea is to figure out how many turns of
    # the circles are needed for them all to line up at the end.

    # Find the unique cycle length from each start and instruction offset.
    var
      cycles = initTable[Address, var IntSet]()
      cursor: Address
      steps: int
      ip: int
    for start in starts:
      cycles[start] = initIntSet()
    for start in starts:
      cursor = start
      for iStart in 0..instructions.high:
        ip = iStart
        steps = 0
        while not cursor.isEnd:
          case instructions[ip]:
            of 'L':
              cursor = graph[cursor].left
            of 'R':
              cursor = graph[cursor].right
            else:
              doAssert false
          ip = (ip + 1) mod instructions.len
          steps += 1
        cycles[start].incl steps
    for k in cycles.keys:
      cycles[k].excl 0

    echo cycles
    var firstCycles: seq[int] = @[]
    for k, v in cycles:
      firstCycles.add v.items.toSeq[0]
    firstCycles.lcm

  verifyPart(1, 21389)
  verifyPart(2, 21083806112641)
