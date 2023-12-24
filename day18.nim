import aocd
import std/[algorithm, strutils, strscans, strformat, sequtils, sets, tables, unittest]

const example {.used.} = """
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
"""

type
  Direction = enum
    Right = "R"
    Down = "D"
    Left = "L"
    Up = "U"

  Pos = object
    y, x: int

func `+`(a, b: Pos): Pos = Pos(y: a.y + b.y, x: a.x + b.x)
func `*`(a: Pos, d: int): Pos = Pos(y: a.y * d, x: a.x * d)

func manhattan(a, b: Pos): int =
  abs(a.x - b.x) + abs(a.y - b.y)

func shoelace(nodes: seq[Pos]): int =
  var border = 0
  var interior = 0
  for i in 0..<nodes.high:
    let a = nodes[i]
    let b = nodes[succ i]
    border += manhattan(a, b)
    interior += (a.x * b.y - a.y * b.x)
  (interior div 2) + (border div 2) + 1

const Directions = {"R": Right, "L": Left, "U": Up, "D": Down}.toTable
const Delta = {
  Up:    Pos(y: -1, x:  0),
  Down:  Pos(y:  1, x:  0),
  Left:  Pos(y:  0, x: -1),
  Right: Pos(y:  0, x:  1),
}.toTable

day 18:
  part 1:
    var pos: Pos
    let nodes: seq[Pos] = block:
      var nodes = @[Pos(y: 0, x: 0)]
      for line in input.strip.splitLines:
        let (ok, dir, meters, _) = line.scanTuple("$w $i (#$+)$.")
        assert ok
        pos = pos + (Delta[Directions[dir]] * meters)
        nodes.add pos
      nodes
    return shoelace(nodes)

  part 2:
    var pos: Pos
    let nodes: seq[Pos] = block:
      var nodes = @[Pos(y: 0, x: 0)]
      for line in input.strip.splitLines:
        let (ok, _, _, color) = line.scanTuple("$w $i (#$+)$.")
        let meters = color[0..^2].parseHexInt
        let dir = color[^1..^1].parseHexInt
        assert ok
        pos = pos + (Delta[Direction(dir)] * meters)
        nodes.add pos
      nodes
    return shoelace(nodes)

  verifyPart(1, 49061)
  verifyPart(2, 92556825427032)
