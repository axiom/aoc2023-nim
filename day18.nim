import aocd
import std/[strutils, strscans, strformat, sequtils, heapqueue, sets, tables, unittest]

type
  Direction = enum
    Up = "U"
    Down = "D"
    Left = "L"
    Right = "R"

  Instruction = object
    dir: Direction
    meters: int
    color: int

  Pos = object
    y, x: int

  Node = object
    color: int
    pos: Pos

  Puzzle = object
    width, height: int
    nodes: Table[Pos, Node]

  Grid = seq[seq[Node]]

func `+`(a, b: Pos): Pos = Pos(y: a.y + b.y, x: a.x + b.x)
func `*`(a: Pos, d: int): Pos = Pos(y: a.y * d, x: a.x * d)

func `->`(p: Pos, d: Direction): Pos =
  case d
  of Up:    p + Pos(y: -1, x:  0)
  of Down:  p + Pos(y:  1, x:  0)
  of Left:  p + Pos(y:  0, x: -1)
  of Right: p + Pos(y:  0, x:  1)

func red(s: string): string = "\x1b[31m" & s & "\x1b[0m"

func `$`(n: Node): string =
  if n.color == 0:
    "."
  elif n.color == 1:
    red "#"
  else:
    "#"

func `$`(puzzle: Puzzle): string =
  for y in 0..<puzzle.height:
    for x in 0..<puzzle.width:
      let p = puzzle.nodes.getOrDefault(Pos(y: y, x: x), Node(color: 0, pos: Pos(y: y, x: x)))
      result &= $p
    result &= "\n"

func parseDir(s: string): Direction =
  case s
  of "R": Right
  of "L": Left
  of "D": Down
  of "U": Up
  else:
    assert false
    Up

func floodFill(p: Puzzle): Puzzle =
  var filled = p.nodes
  let start = Pos(y: p.height div 2, x: p.width div 3)
  var graph = @[start].toSet
  var seen = @[start].toSet
  var count = 0
  while graph.len > 0:
    inc count
    if count mod 10000 == 0:
      debugEcho fmt"graph.len = {graph.len} filled.len = {filled.len}"

    let n = graph.pop
    seen.incl n
    if n notin p.nodes:
      filled[n] = Node(color: 1, pos: n)
      for d in [Up, Down, Left, Right]:
        let pos = n -> d
        if pos notin seen and pos.y in 0..<p.height and pos.x in 0..<p.width:
          graph.incl pos

  filled[start] = Node(color: 1, pos: start)
  Puzzle(nodes: filled, width: p.width, height: p.height)

func volume(puzzle: Puzzle): int =
  puzzle.nodes.len

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

day 18:
  let puzzle = input.strip.splitLines.mapIt:
    let (ok, dir, meters, color) = it.scanTuple("$w $i (#$+)$.")
    assert ok
    Instruction(dir: parseDir(dir), meters: meters, color: parseHexInt(color))

  part 1:
    var x, y: int
    var minx, maxx, miny, maxy: int
    var pos: Pos
    var nodes: seq[Node]

    for instr in puzzle:
      for m in 1..instr.meters:
        pos = pos -> instr.dir
        nodes.add Node(color: instr.color, pos: pos)

    for node in nodes:
      minx = min(minx, node.pos.x)
      maxx = max(maxx, node.pos.x)
      miny = min(miny, node.pos.y)
      maxy = max(maxy, node.pos.y)

    var width = maxx - minx + 1
    var height = maxy - miny + 1

    for node in nodes.mitems:
      node.pos.x -= minx
      node.pos.y -= miny

    var myNodes: Table[Pos, Node]
    for node in nodes:
      myNodes[node.pos] = node

    var grid: Grid
    for y in 0..<height:
      grid.add newSeq[Node](width)
      for x in 0..<width:
        let node = myNodes.getOrDefault(Pos(y: y, x: x), Node(color: 0, pos: Pos(y: y, x: x)))
        grid[y][x] = node

    let puz = Puzzle(nodes: myNodes, width: width, height: height)

    echo $puz
    let filled = floodFill puz
    echo $filled
    volume filled


  part 2:
    0

  verifyPart(1, 49061)
  # verifyPart(2, 94)
