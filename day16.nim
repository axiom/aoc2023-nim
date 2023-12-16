import aocd
import std/[strutils, sequtils, sets, tables, unittest]

type
  Tile = enum
    Empty = "·"
    RMirror = "╱"
    LMirror = "╲"
    VSplit = "│"
    HSplit = "─"
    Energized = "#"
  Grid = object
    height, width: int
    tiles: seq[seq[Tile]]
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

func `[]`(grid: Grid, pos: Pos): Tile =
  grid.tiles[pos.y][pos.x]

func `[]=`(grid: var Grid, pos: Pos, tile: Tile) =
  grid.tiles[pos.y][pos.x] = tile

func contains(grid: Grid, b: Beam): bool =
  ## Check if the beam's position is within the grid
  b.pos.y in 0..<grid.height and b.pos.x in 0..<grid.width

func `->`(p: Pos, d: Direction): Pos =
  ## Move position in the given direction
  case d
  of Up:    Pos(x: p.x,     y: p.y - 1)
  of Down:  Pos(x: p.x,     y: p.y + 1)
  of Left:  Pos(x: p.x - 1, y: p.y)
  of Right: Pos(x: p.x + 1, y: p.y)

func `->`(b: Beam, d: Direction): Beam =
  ## Redirect the beam in the given direction
  Beam(pos: b.pos -> d, dir: d)

func `=>`(b: Beam, d: Direction): Beams =
  ## Redirect the beam in the given direction
  Beams(multiple: false, beam: Beam(pos: b.pos -> d, dir: d))

func `=|`(b: Beam, d: Direction): Beams =
  ## Split the beam from the given direction
  case d
  of Left, Right:
    Beams(multiple: true, beams: [b -> Up, b -> Down])
  of Up, Down:
    Beams(multiple: true, beams: [b -> Left, b -> Right])

func `=>`(b: Beam, s: Tile): Beams =
  ## Divert the beam according to the given tile
  case s
  of Empty: b => b.dir
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
    of Up, Down:    b => b.dir
    of Left, Right: b =| b.dir
  of HSplit:
    case b.dir
    of Up, Down:    b =| b.dir
    of Left, Right: b => b.dir
  of Energized:
    assert false
    b => Left

func shine(grid: Grid, incoming: Beam): BeamPath =
  ## Shine the incoming beam through the grid
  var beams = @[incoming]
  while beams.len > 0:
    let beam = beams.pop

    if beam in result or beam notin grid:
      continue

    result.incl beam

    case (let bs = beam => grid[beam.pos]; bs.multiple)
    of true:
      beams.add bs.beams[0]
      beams.add bs.beams[1]
    of false:
      beams.add bs.beam

func positions(beamPath: BeamPath): HashSet[Pos] =
  beamPath.mapIt(it.pos).toHashSet

iterator incomingBeams(grid: Grid): Beam =
  ## Generate all possible incoming beams.
  for x in 0..<grid.width:
    yield Beam(pos: Pos(x: x, y: 0), dir: Down)
    yield Beam(pos: Pos(x: x, y: grid.height - 1), dir: Up)
  for y in 0..<grid.height:
    yield Beam(pos: Pos(x: 0, y: y), dir: Right)
    yield Beam(pos: Pos(x: grid.width - 1, y: y), dir: Left)

func bestIncomingBeam(grid: Grid): Beam =
  ## Find the best incoming beam producing the most number of energized tiles.
  var mostEnergized = 0
  for beam in grid.incomingBeams:
    let energizedPositions = grid.shine(beam).positions.len
    if energizedPositions > mostEnergized:
      mostEnergized = energizedPositions
      result = beam

func overlay(grid: Grid, beamPath: BeamPath, onlyEmpty: bool): Grid =
  var overlayed = grid
  for pos in beamPath.positions:
    if not onlyEmpty or grid[pos] == Empty:
      overlayed[pos] = Energized
  overlayed

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
  for y in 0..<grid.height:
    for x in 0..<grid.width:
      result &= $grid[Pos(x: x, y: y)]
    result &= "\n"


day 16:
  const CharMap = {'.': Empty, '/': RMirror, '\\': LMirror, '|': VSplit, '-': HSplit}.toTable
  let puzzle: Grid = block:
    var tiles: seq[seq[Tile]]
    var height, width: int
    for line in input.strip.splitLines:
      inc height
      width = line.len
      var row: seq[Tile]
      for x, c in line.pairs:
        row.add CharMap[c]
      tiles.add row
    Grid(height: height, width: width, tiles: tiles)

  part 1:
    let beamPath = puzzle.shine Beam(pos: Pos(x: 0, y: 0), dir: Right)
    result = beamPath.positions.len

  part 2:
    let bestBeam = puzzle.bestIncomingBeam
    let beamPath = puzzle.shine(bestBeam)
    # echo $(puzzle.overlay(beamPath, true))
    result = beamPath.positions.len

  verifyPart(1, 7236)
  verifyPart(2, 7521)
