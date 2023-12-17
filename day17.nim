import aocd
import std/[strutils, sequtils, heapqueue, sets, tables, unittest]

type
  Grid = seq[seq[uint8]]
  Direction = enum
    None = "#", Up = "↑", Down = "↓", Left = "←", Right = "→"
  Pos = object
    y, x: int
    dir: Direction
    run: int
  Path = object
    pos: Pos
    fScore: int

func red(s: string): string = "\x1b[31m" & s & "\x1b[0m"

func overlay(g: Grid, path: Table[(int, int), Direction]): string =
  for y in 0..g.high:
    for x in 0..g[0].high:
      if (y, x) in path:
        result &= red $path[(y, x)]
      else:
        result &= $g[y][x]
    result &= "\n"


func `<`(a, b: Path): bool = a.fScore < b.fScore

func `->`(pos: Pos, dir: Direction): Pos =
  let run = if pos.dir == dir: succ pos.run else: 1
  case dir
  of Up:    Pos(y: pos.y - 1, x: pos.x,     dir: dir, run: run)
  of Down:  Pos(y: pos.y + 1, x: pos.x,     dir: dir, run: run)
  of Left:  Pos(y: pos.y,     x: pos.x - 1, dir: dir, run: run)
  of Right: Pos(y: pos.y,     x: pos.x + 1, dir: dir, run: run)
  else: pos

const Turns = {
  Up:    @[Up, Left, Right],
  Down:  @[Down, Left, Right],
  Left:  @[Left, Up, Down],
  Right: @[Right, Up, Down],
  None:  @[Up, Down, Left, Right],
}.toTable

func isValidNextDirection(pos: Pos; dir: Direction; minRun, maxRun: int): bool =
  let
    curr = dir
    prev = pos.dir

  if prev == None:
    return true
  elif pos.run < minRun:
    return curr == prev
  elif pos.run >= maxRun:
    return curr != prev
  else:
    return true

func contains(g: Grid, p: Pos): bool =
  result = p.y in 0..g.high and p.x in 0..g[0].high

func reconstructPath(cameFrom: Table[Pos, (Pos, Direction)], current: Pos): Table[(int, int), Direction] =
  var current = current
  result = {(current.y, current.x): None}.toTable
  while current in cameFrom:
    let (pos, dir) = cameFrom[current]
    current = pos
    result[(current.y, current.x)] = dir

func solve(g: Grid, minRun, maxRun: int): int =
  let
    goal = Pos(y: g.high, x: g[0].high)
    start = Pos(y: 0, x: 0, run: 0, dir: None)
  var
    cameFrom: Table[Pos, (Pos, Direction)]
    gScore: Table[Pos, int]
    openSet: HashSet[Pos]
    frontier: HeapQueue[Path]
    best: Pos

  func h(p: Pos): int = abs(goal.y - p.y) + abs(goal.x - p.x)
  func d(p: Pos): int = g[p.y][p.x].int

  openSet.incl start
  frontier.push Path(pos: start, fScore: h(start))
  gScore[start] = 0
  result = int.high

  while frontier.len > 0:
    let current = frontier.pop
    openSet.excl current.pos

    if current.pos.y == goal.y and current.pos.x == goal.x and current.pos.run >= minRun:
      if gScore[current.pos] < result:
        result = gScore[current.pos]
        best = current.pos
        # TODO: Will this not skip a better option?
        break

    for dir in Turns[current.pos.dir]:
      let neighbor = current.pos -> dir

      if neighbor notin g:
        continue

      if neighbor in openset:
        continue

      if not isValidNextDirection(current.pos, dir, minRun, maxRun):
        continue

      let tentativeGScore = gScore.getOrDefault(current.pos, int.high) + d(neighbor)
      if tentativeGScore < gScore.getOrDefault(neighbor, int.high):
        gScore[neighbor] = tentativeGScore
        cameFrom[neighbor] = (current.pos, current.pos.dir)
        openSet.incl neighbor
        frontier.push Path(pos: neighbor, fScore: tentativeGScore + h(neighbor))

  # let path = reconstructPath(cameFrom, best)
  # debugEcho $g.overlay(path)

const example1 {.used.} = """
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
"""

const example2 {.used.} = """
111111111111
999999999991
999999999991
999999999991
999999999991
"""

day 17:
  let puzzle: Grid = input.strip.splitLines.mapIt(it.mapIt(parseInt($it).uint8))

  part 1:
    solve(puzzle, -1, 3)

  part 2:
    solve(puzzle, 4, 10)

  verifyPart(1, 902)
  verifyPart(2, 1073)
  # verifyPart(1, 102)
  # verifyPart(2, 94)
