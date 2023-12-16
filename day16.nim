import aocd
import std/[strutils, strformat, strscans, sequtils, sets, tables, unittest]

type
  Squer = enum
    Empty = "."
    RMirror = "/"
    LMirror = "\\"
    VSplit = "|"
    HSplit = "-"
    Energized = "#"
  Grid = seq[seq[Squer]]
  Direction = enum
    Up, Down, Left, Right
  Pos = object
    x, y: int
  Beam = object
    pos: Pos
    dir: Direction
  BeamPath = HashSet[Beam]
  Beams = object
    case multiple: bool
    of true: beams: array[2, Beam]
    of false: beam: Beam

const CharMap = {'.': Empty, '/': RMirror, '\\': LMirror, '|': VSplit, '-': HSplit}.toTable

func at(grid: Grid, pos: Pos): Squer = grid[pos.y][pos.x]

func contains(grid: Grid, b: Beam): bool =
  b.pos.y in 0..grid.high and b.pos.x in 0..grid[0].high

func `->`(p: Pos, d: Direction): Pos =
  case d
  of Up:    Pos(x: p.x,     y: p.y - 1)
  of Down:  Pos(x: p.x,     y: p.y + 1)
  of Left:  Pos(x: p.x - 1, y: p.y)
  of Right: Pos(x: p.x + 1, y: p.y)

func `->`(b: Beam, d: Direction): Beam =
  result = Beam(pos: b.pos -> d, dir: d)

func `=>`(b: Beam, d: Direction): Beams =
  result = Beams(multiple: false, beam: Beam(pos: b.pos -> d, dir: d))

func split(b: Beam, d1: Direction, d2: Direction): Beams =
  result = Beams(multiple: true, beams: [b -> d1, b -> d2])

func `=>`(b: Beam, s: Squer): Beams =
  case s
  of Empty:
    # Beam just continues on its direction
    b => b.dir
  of RMirror:
    case b.dir
    of Up:    b => Right
    of Down:  b => Left
    of Left:  b => Down
    of Right: b => Up
  of LMirror:
    case b.dir
    of Up:    b => Left
    of Down:  b => Right
    of Left:  b => Up
    of Right: b => Down
  of VSplit:
    case b.dir
    of Up, Down: b => b.dir
    of Left, Right: b.split(Up, Down)
  of HSplit:
    case b.dir
    of Up, Down: b.split(Left, Right)
    of Left, Right: b => b.dir
  of Energized:
    assert false
    b => Left

proc shine(grid: Grid, incoming: Beam): BeamPath =
  var beams = @[incoming]
  var count = 0
  while beams.len > 0:
    let beam = beams.pop

    if beam in result:
      continue

    # Check if beam has left the grid
    if beam notin grid:
      continue

    result.incl beam
    inc count

    let bs = beam => grid.at(beam.pos)
    case bs.multiple
    of true:
      beams.add(bs.beams[0])
      beams.add(bs.beams[1])
    of false:
      beams.add(bs.beam)

func positions(beamPath: BeamPath): HashSet[Pos] =
  for beam in beamPath:
    result.incl beam.pos

proc bestIncoming(grid: Grid): Beam =
  ## Find the best incoming beam producing the most number of energized tiles.
  var candidates: seq[Beam]
  for x in 0..grid[0].high:
    candidates.add(Beam(pos: Pos(x: x, y: 0), dir: Down))
    candidates.add(Beam(pos: Pos(x: x, y: grid.high), dir: Up))
  for y in 0..grid.high:
    candidates.add(Beam(pos: Pos(x: 0, y: y), dir: Right))
    candidates.add(Beam(pos: Pos(x: grid[0].high, y: y), dir: Left))

  var mostEnergized = 0
  for candidate in candidates:
    let beamPath = grid.shine(candidate)
    let positions = beamPath.positions
    if positions.len > mostEnergized:
      mostEnergized = positions.len
      result = candidate

func overlay(grid: Grid, beamPath: BeamPath, onlyEmpty: bool): Grid =
  var result = grid
  for pos in beamPath.positions:
    if not onlyEmpty or result[pos.y][pos.x] == Empty:
      result[pos.y][pos.x] = Energized
  result

const example {.used.} = """
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
""".strip

func `$`(grid: Grid): string =
  for row in grid:
    for squer in row:
      result &= $squer
    result &= "\n"

day 16:
  let puzzle: Grid = input.strip.splitLines.mapIt:
    it.mapIt:
      if it in CharMap:
        CharMap[it]
      else:
        assert false
        Empty

  part 1:
    let beamPath = puzzle.shine Beam(pos: Pos(x: 0, y: 0), dir: Right)
    result = beamPath.positions.len

  part 2:
    let bestBeam = puzzle.bestIncoming
    let beamPath = puzzle.shine(bestBeam)
    result = beamPath.positions.len

  verifyPart(1, 7236)
  verifyPart(2, 7521)
