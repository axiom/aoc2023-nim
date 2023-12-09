import aocd
import std/[strutils, math, strscans, tables, sequtils, unittest]

const example1 {.used.} = """
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"""

const example2 {.used.} = """
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"""

const example3 {.used.} = """
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
  Address = string
  Node = object
    left: Address
    right: Address

day 8:
  let
    instructions = inputLines[0]
  var
    graph: Table[Address, Node]

  # Build up the graph
  for line in inputLines:
    if (let (ok, a, l, r) = line.scanTuple("$+ = ($+, $+)$."); ok):
      graph[a] = Node(left: l, right: r)

  part 1:
    var cursor = "AAA"
    result = 0
    while cursor != "ZZZ":
      case instructions[result mod instructions.len]:
        of 'L': cursor = graph[cursor].left
        of 'R': cursor = graph[cursor].right
        else: doAssert false
      inc result

  part 2:
    # Having looked at a graph of the data, it is clear that we have a "circle"
    # graph for each entry node. So thea idea is to figure out how many turns of
    # the circles are needed for them all to line up at the end.

    var
      cycles: seq[int]
      cursor: Address
      steps: int

    for start in graph.keys:
      if start[2] != 'A':
        continue
      cursor = start
      for i in 0..instructions.high:
        steps = 0
        while cursor[2] != 'Z':
          case instructions[(steps + i) mod instructions.len]:
            of 'L': cursor = graph[cursor].left
            of 'R': cursor = graph[cursor].right
            else: doAssert false
          inc steps
        if steps != 0:
          cycles.add steps

    # > The least common multiple of more than two integers a, b, c, . . . ,
    # > usually denoted by lcm(a, b, c, . . .), is defined as the smallest
    # > positive integer that is divisible by each of a, b, c,
    cycles.lcm

  verifyPart(1, 21389)
  verifyPart(2, 21083806112641)
