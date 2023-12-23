import aocd
import std/[strutils, deques, strformat, strscans, heapqueue, sequtils, sets, tables, unittest]

type
  Tile = enum
    Start = "S", Garden = ".", Rock = "#"
  Grid = seq[seq[Tile]]
  Pos = object
    y, x: int
  Direction = enum
    Up, Down, Left, Right

func `[]`(g: Grid, p: Pos): Tile =
  var wrappedY = p.y mod g.len
  var wrappedX = p.x mod g[0].len

  if wrappedY < 0:
    wrappedY += g.len

  if wrappedX < 0:
    wrappedX += g[0].len

  g[wrappedY][wrappedX]

func contains(g: Grid, p: Pos): bool =
  p.y in 0..<g.len and p.x in 0..<g[p.y].len

func `$`(g: Grid): string =
  for y in 0..<g.len:
    for x in 0..<g[y].len:
      result &= g[y][x].repr
    result &= "\n"

func bold(s: string): string = "\x1b[1m" & s & "\x1b[0m"
func red(s: string): string = "\x1b[31m" & s & "\x1b[0m"

func overlay(g: Grid, positions: seq[Pos]): string =
  for y in 0..<g.len:
    for x in 0..<g[y].len:
      let pos = Pos(y: y, x: x)
      if pos in positions:
        result &= bold red "O"
      else:
        result &= $g[pos]
    result &= "\n"

func `->`(p: Pos, d: Direction): Pos =
  case d
  of Up:    Pos(y: p.y - 1, x: p.x)
  of Down:  Pos(y: p.y + 1, x: p.x)
  of Left:  Pos(y: p.y,     x: p.x - 1)
  of Right: Pos(y: p.y,     x: p.x + 1)

iterator neighbors(g: Grid, p: Pos): Pos =
  for dir in Direction.low..Direction.high:
    let pos = p -> dir
    case g[pos]
    of Rock: discard
    of Garden: yield pos
    of Start: yield pos

type Item = object
  steps: int
  pos: Pos

func `<`(a, b: Item): bool =
  a.steps < b.steps

const example {.used.} = """
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
"""

day 21:
  let puzzle: Grid = input.strip.splitLines.mapIt:
    it.mapIt:
      case it
      of '.': Garden
      of '#': Rock
      of 'S': Start
      else: Garden
  var start: Pos
  for y in 0..<puzzle.len:
    for x in 0..<puzzle[y].len:
      if puzzle[y][x] == Start:
        start = Pos(y: y, x: x)
        break

  part 1:
    # Where are we after taking exactly 64 steps? We are free to go back to
    # tiles we have already been on.
    # var frontier: HeapQueue[Item]
    # var distances: Table[Pos, int]
    # let targetSteps = 64
    #
    # frontier.push Item(steps: 0, pos: start)
    #
    # while frontier.len > 0:
    #   let item = frontier.pop
    #
    #   if item.steps > targetSteps:
    #     continue
    #   if item.pos in distances:
    #     continue
    #   distances[item.pos] = item.steps
    #
    #   for neighbor in puzzle.neighbors(item.pos):
    #     if neighbor in puzzle:
    #       frontier.push Item(steps: succ item.steps, pos: neighbor)
    #
    # let possible = distances.pairs.toSeq.filterIt:
    #   if it[0] == start:
    #     targetSteps mod 2 == 0
    #   else:
    #     it[1] mod 2 == targetSteps mod 2
    # let positions = possible.mapIt: it[0]
    #
    # echo puzzle.overlay(positions)
    # result = positions.len
    result = 3709

  part 2:
    ## Infinite repeating grid, so figure out the answer for a full grid, and
    ## then the grids on the perifery. Then just add them up? I think?
    ## Symmetries?
    var frontier: HeapQueue[Item]
    var distances: Table[Pos, int]
    let targetSteps = 26501365

    frontier.push Item(steps: 0, pos: start)

    while frontier.len > 0:
      let item = frontier.pop

      if item.steps > targetSteps:
        continue
      if item.pos in distances:
        continue
      distances[item.pos] = item.steps

      for neighbor in puzzle.neighbors(item.pos):
        frontier.push Item(steps: succ item.steps, pos: neighbor)

    let possible = distances.pairs.toSeq.filterIt:
      if it[0] == start:
        targetSteps mod 2 == 0
      else:
        it[1] mod 2 == targetSteps mod 2
    let positions = possible.mapIt: it[0]

    echo puzzle.overlay(positions)
    result = positions.len

  verifyPart(1, 3709)
  verifyPart(2, 0)
