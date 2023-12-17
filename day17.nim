import aocd
import std/[strutils, strformat, sequtils, heapqueue, sets, tables, unittest]

type
  Grid = seq[seq[uint8]]
  Direction = enum
    None = "#", Up = "↑", Down = "↓", Left = "←", Right = "→"
  Pos = object
    y, x: int
    dirs: array[10, Direction]
  Path = object
    pos: Pos
    fScore: int

func `$`(g: Grid): string =
  result = g.mapIt(it.mapIt($it).join).join("\n")

func lastDir(pos: Pos): Direction = pos.dirs[0]

func lastDir(path: Path): Direction = path.pos.lastDir

func run(pos: Pos): int =
  let steps = pos.dirs.toSeq.filterIt(it != None)

  if steps.len <= 1:
    return steps.len

  result = 1
  for i in 1..steps.high:
    if steps[i] == steps[i - 1]:
      inc result
    else:
      break

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
  let dirs = [dir, pos.dirs[0], pos.dirs[1], pos.dirs[2], pos.dirs[3], pos.dirs[4], pos.dirs[5], pos.dirs[6], pos.dirs[7], pos.dirs[8]]
  case dir
  of Up:    Pos(y: pos.y - 1, x: pos.x,     dirs: dirs)
  of Down:  Pos(y: pos.y + 1, x: pos.x,     dirs: dirs)
  of Left:  Pos(y: pos.y,     x: pos.x - 1, dirs: dirs)
  of Right: Pos(y: pos.y,     x: pos.x + 1, dirs: dirs)
  else: pos

func opposite(dir: Direction): Direction =
  case dir
  of Up:    Down
  of Down:  Up
  of Left:  Right
  of Right: Left
  of None:  None

func isValidNextDirection(pos: Pos, dir: Direction): bool =
  let
    curr = dir
    prev = pos.dirs[0]

  let turns = pos.dirs.toSeq.filterIt(it != None)
  let turned = curr != prev and prev != None

  if turned and curr == opposite(prev):
    return false

  if pos.run < 4:
    return not turned
  elif pos.run >= 10:
    return turned
  else:
    return true

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

    if current.pos.y == goal.y and current.pos.x == goal.x and current.pos.run >= 4:
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
    echo $puzzle
    result = solve puzzle
    echo result

  part 2:
    result = 0

  verifyPart(1, 902)
  # verifyPart(2, 1073)
