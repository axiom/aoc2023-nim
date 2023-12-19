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

  Node = int

  Bounds = object
    xlow, xhigh, ylow, yhigh: int

  Puzzle = object
    bounds: Bounds
    nodes: HashSet[Pos]

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
  if n == 0:
    "."
  elif n == 1:
    red "#"
  else:
    "#"

func `$`(puzzle: Puzzle): string =
  let b = puzzle.bounds
  for y in b.ylow..b.yhigh:
    for x in b.xlow..b.xhigh:
      let p = Pos(y: y, x: x)
      if p in puzzle.nodes:
        result &= "#"
      else:
        result &= "."
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
  let b = p.bounds
  let start = Pos(y: b.yhigh div 2, x: b.xhigh div 3)
  var graph = @[start].toHashSet
  var seen = @[start].toHashSet
  var count = 0
  while graph.len > 0:
    inc count
    if count mod 10000 == 0:
      debugEcho fmt"graph.len = {graph.len} filled.len = {filled.len}"

    let n = graph.pop
    seen.incl n
    if n notin p.nodes:
      filled.incl n
      for d in [Up, Down, Left, Right]:
        let pos = n -> d
        if pos notin seen and pos.y in b.ylow..<b.yhigh and pos.x in b.xlow..<b.xhigh:
          graph.incl pos

  filled.incl start
  Puzzle(nodes: filled, bounds: p.bounds)

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
    var nodes: HashSet[Pos]

    for instr in puzzle:
      for m in 1..instr.meters:
        pos = pos -> instr.dir
        nodes.incl pos

    for pos in nodes:
      minx = min(minx, pos.x)
      maxx = max(maxx, pos.x)
      miny = min(miny, pos.y)
      maxy = max(maxy, pos.y)

    let puz = Puzzle(nodes: nodes, bounds: Bounds(xlow: minx, xhigh: maxx, ylow: miny, yhigh: maxy))

    let filled = floodFill puz
    let v: int64 = volume filled
    v

  part 2:
    var x, y: int
    var minx, maxx, miny, maxy: int
    var pos: Pos
    var nodes: HashSet[Pos]

    for instr in puzzle:
      echo fmt"nodes = {nodes.len}"
      for m in 1..instr.meters:
        pos = pos -> instr.dir
        nodes.incl pos

    for pos in nodes:
      minx = min(minx, pos.x)
      maxx = max(maxx, pos.x)
      miny = min(miny, pos.y)
      maxy = max(maxy, pos.y)

    let puz = Puzzle(nodes: nodes, bounds: Bounds(xlow: minx, xhigh: maxx, ylow: miny, yhigh: maxy))

    let filled = floodFill puz
    let v: int64 = volume filled
    v

  verifyPart(1, 49061)
  # verifyPart(2, 94)
