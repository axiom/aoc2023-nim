import aocd
import unicode except strip
import std/[strutils, strscans, tables, sequtils, re, intsets, unittest]

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

type
  Address = int
  Node = tuple[left: int, right: int]

func encode(str: string): Address =
  for c in str:
    let v = c.ord - 'A'.ord
    result = (result shl 5) or v

template step(graph: Table[Address, Node], instruction: char, cursors: var seq[Address]): bool =
  for i, cursor in cursors:
    let (left, right) = graph[cursor]
    case instructions[ip]:
      of 'L':
        cursors[i] = left
      of 'R':
        cursors[i] = right
      else:
        doAssert false

template finished(cursors: seq[Address]): bool =
  for cursor in cursors:
    if cursor[2] != 'Z':
      return false
  true

day 8:
  let xlines = input.strip.splitLines
  let instructions = xlines[0]
  var cursors: seq[Address] = @[]
  var graph = initTable[Address, Node]()
  for line in xlines[2..^1]:
    let (ok, a, l, r) = line.scanTuple("$w = ($w, $w)$.")
    doAssert ok
    graph[encode(a)] = (left: encode(l), right: encode(r))
    if a[2] == 'A':
      cursors.add encode(a)

  part 1:
    let start = "AAA".encode
    let target = "ZZZ".encode

    var steps = 0
    var ip = 0
    var cursor = start
    while cursor != target:
      steps += 1
      if steps mod 1_000_000 == 0:
        echo steps
      let (left, right) = graph[cursor]
      case instructions[ip]:
        of 'L':
          cursor = left
        of 'R':
          cursor = right
        else:
          doAssert false
      ip = (ip + 1) mod instructions.len
    steps

  part 2:
    3

  verifyPart(1, 21389)
  verifyPart(2, 15455663)
