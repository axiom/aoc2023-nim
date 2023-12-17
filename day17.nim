import aocd
import std/[strutils, strformat, sequtils, heapqueue, sets, tables, unittest]

type
  Grid = seq[seq[uint8]]
  Direction = enum
    None = "#", Up = "↑", Down = "↓", Left = "←", Right = "→"
  Pos = object
    y, x: int
    dirs: array[3, Direction]
  Path = object
    pos: Pos
    fScore: int

func `$`(g: Grid): string =
  result = g.mapIt(it.mapIt($it).join).join("\n")

func lastDir(pos: Pos): Direction = pos.dirs[0]

func lastDir(path: Path): Direction = path.pos.lastDir

func overlay(g: Grid, path: seq[(Pos, Direction)]): string =
  for y in 0..g.high:
    for x in 0..g[0].high:
      let p = path.filterIt(it[0].x == x and it[0].y == y)
      if p.len > 0:
        result &= $p[0][1]
      else:
        result &= $g[y][x]
    result &= "\n"

func manhattan(a, b: Pos): int =
  abs(a.y - b.y) + abs(a.x - b.x)

func `<`(a, b: Path): bool =
  a.fScore < b.fScore

func `->`(pos: Pos, dir: Direction): Pos =
  case dir
  of Up:    Pos(y: pos.y - 1, x: pos.x,     dirs: [dir, pos.dirs[0], pos.dirs[1]])
  of Down:  Pos(y: pos.y + 1, x: pos.x,     dirs: [dir, pos.dirs[0], pos.dirs[1]])
  of Left:  Pos(y: pos.y,     x: pos.x - 1, dirs: [dir, pos.dirs[0], pos.dirs[1]])
  of Right: Pos(y: pos.y,     x: pos.x + 1, dirs: [dir, pos.dirs[0], pos.dirs[1]])
  else: pos

func opposite(dir: Direction): Direction =
  case dir
  of Up:    Down
  of Down:  Up
  of Left:  Right
  of Right: Left
  of None:  None

func isValidNextDirection(pos: Pos, dir: Direction): bool =
  if pos.lastDir == None:
    return true
  if pos.lastDir.opposite == dir:
    return false
  dir != pos.dirs[0] or pos.dirs[0] != pos.dirs[1] or pos.dirs[1] != pos.dirs[2]

func contains(g: Grid, p: Pos): bool =
  result = p.y in 0..g.high and p.x in 0..g[0].high

func reconstructPath(cameFrom: Table[Pos, (Pos, Direction)], current: Pos): seq[(Pos, Direction)] =
  var current = current
  result = @[(current, None)]
  while current in cameFrom:
    let (pos, dir) = cameFrom[current]
    current = pos
    result.add (current, dir)

func solve(g: Grid): int =
  let
    goal = Pos(y: g.high, x: g[0].high)
    start = Pos(y: 0, x: 0)
  var
    cameFrom: Table[Pos, (Pos, Direction)]
    gScore: Table[Pos, int]
    openSet: HashSet[Pos]
    frontier: HeapQueue[Path]
    best: Pos

  openSet.incl start
  frontier.push Path(pos: start, fScore: start.manhattan(goal))
  gScore[start] = 0
  result = int.high

  func h(p: Pos): int = p.manhattan(goal)
  func d(p: Pos): int = g[p.y][p.x].int

  while frontier.len > 0:
    let current = frontier.pop
    openSet.excl current.pos

    if current.pos.y == goal.y and current.pos.x == goal.x:
      if gScore[current.pos] < result:
        result = gScore[current.pos]
        best = current.pos

    for dir in [Up, Down, Left, Right]:
      # Exclude 180 degree turns
      if not current.pos.isValidNextDirection(dir):
        continue

      let neighbor = current.pos -> dir
      # Exclude off grid positions
      if neighbor notin g:
        continue

      let tentativeGScore = gScore.getOrDefault(current.pos, int.high) + d(neighbor)
      if tentativeGScore < gScore.getOrDefault(neighbor, int.high):
        gScore[neighbor] = tentativeGScore
        cameFrom[neighbor] = (current.pos, current.pos.dirs[0])
        let fScore = tentativeGScore + h(neighbor)
        if neighbor notin openSet:
          openSet.incl neighbor
          frontier.push Path(pos: neighbor, fScore: fScore)

  let path = reconstructPath(cameFrom, best)
  debugEcho ""
  debugEcho $g.overlay(path)

const example {.used.} = """
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

day 17:
  let puzzle: Grid = input.strip.splitLines.mapIt(it.mapIt(parseInt($it).uint8))

  part 1:
    echo $puzzle
    result = solve puzzle
    echo result

  part 2:
    result = 0

  verifyPart(1, 902)
  # verifyPart(2, 246762)
