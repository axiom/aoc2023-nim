import aocd
import std/[strutils, sets, tables, sequtils, unittest]
import pixie

type
  Grid = seq[string]
  Pos = object
    r, c: int
  Dir = enum
    North, South, East, West

func charAt(g: Grid, p: Pos, default: char = '.'): char =
  ## Get the character at a position, or '.' if out of bounds.
  if p.r in g.low..g.high and p.c in g[0].low..g[0].high:
    return g[p.r][p.c]
  default

func `+`(p: Pos, d: Dir): Pos =
  ## Generate a new position by moving in a direction.
  case d:
    of North: Pos(r: p.r - 1, c: p.c)
    of South: Pos(r: p.r + 1, c: p.c)
    of East:  Pos(r: p.r,     c: p.c + 1)
    of West:  Pos(r: p.r,     c: p.c - 1)

const
  Wards = {
    North: "J|LS",
    South: "7|FS",
    West: "J-7S",
    East: "F-LS",
  }.toTable

  Opposite = { North: South, South: North, East: West, West: East, }.toTable

func opposite(d: Dir): Dir = Opposite[d]

const fancyChars = {
  'F': "╭", '7': "╮",
  'J': "╯", 'L': "╰",
  '|': "│", '-': "─",
  '.': " ", 'S': "S",
  'O': "#", 'I': "█"
}.toTable

func colored(text, color: string): string =
  ## Color some text using ANSI escape codes.
  "\x1b[38;5;" & color & "m" & text & "\x1b[0m"

func blue(text: string): string = colored(text, "4")
func red(text: string): string = colored(text, "1")
func cyan(text: string): string = colored(text, "3")

proc fancy(c: char): string =
  ## Replace input box symbols with fancier versions.
  let x = fancyChars.getOrDefault(c, $c)
  case c:
    of '|', '-', 'F', '7', 'J', 'L': return cyan(x)
    of 'O': return red(x)
    of 'I': return blue(x)
    else: return x

day 10:
  # Parse and store the input
  let
    grid = input.strip.splitLines
    start = block findStart:
      var start = Pos(r: 0, c: 0)
      for r in 0..grid.high:
        for c in 0..grid[0].high:
          if grid[r][c] == 'S':
            start = Pos(r: r, c: c)
      start

  # Find the looped pipe in the grid
  var
    loop: HashSet[Pos]
    curr = start
    done = false
  loop.incl start
  while not done:
    done = true # done if the loop dosn't find a new position
    for dir in [North, South, East, West]:
      let
        nextPos = curr + dir
        currFitsNext = grid.charAt(curr) in Wards[dir]
        nextFitsCurr = grid.charAt(nextPos) in Wards[opposite dir]

      if currFitsNext and nextFitsCurr and nextPos notin loop:
        curr = nextPos
        done = false
        loop.incl curr
        break

  part 1:
    # Since we are dealing with a loop, the furthest we can go is half the loop.
    loop.len div 2

  part 2:
    var cleanedGrid: seq[string]
    result = 0
    for r in 0..grid.high:
      cleanedGrid.add grid[r]
      var crossings = 0
      var prevPipe = '.'
      for c in 0..grid[r].high:
        var currPipe = grid[r][c]

        # Check for "crossings" of the loop to figure out if we are inside or
        # outside of it. ray scan?
        if Pos(r: r, c: c) in loop:
          if   prevPipe == '.' and currPipe == '|': inc crossings
          elif prevPipe == '|' and currPipe == '|': inc crossings
          elif prevPipe == '7' and currPipe == '|': inc crossings
          elif prevPipe == 'J' and currPipe == '|': inc crossings
          elif prevPipe == 'L' and currPipe == '7': inc crossings
          elif prevPipe == 'L' and currPipe == 'S': inc crossings
          elif prevPipe == 'F' and currPipe == 'J': inc crossings
          elif prevPipe == 'F' and currPipe == 'S': inc crossings

          # Skip horizontal pipes since they don't help us figure out crossings.
          if currPipe != '-': prevPipe = currPipe

          cleanedGrid[r][c] = currPipe
        else:
          if crossings mod 2 == 1:
            cleanedGrid[r][c] = 'I'
            inc result
          else:
            cleanedGrid[r][c] = 'O'

    # Print the cleaned up grid
    for r in 0..cleanedGrid.high:
      for c in 0..cleanedGrid[r].high:
        stdout.write fancy cleanedGrid[r][c]
      echo ""

    # Try to create a visualization using pixie library
    let
      sq = 10.0
      image = newImage(sq.int * grid.len, sq.int * grid.len)
      ctx = newContext(image)
      sprite = readImage("sprite.png")

    image.fill("black")

    for r in 0..cleanedGrid.high:
      for c in 0..cleanedGrid[r].high:
        let
          x = c.float32 * sq
          y = r.float32 * sq
          g = cleanedGrid[r][c]

        case g:
          of 'I': ctx.fillStyle = "blue"
          of 'O': ctx.fillStyle = "red"
          else: ctx.fillStyle = "yellow"

        case g:
          of 'F': ctx.drawImage(sprite,  0,  0, 10, 10, x, y, sq, sq)
          of '-': ctx.drawImage(sprite, 10,  0, 10, 10, x, y, sq, sq)
          of '7': ctx.drawImage(sprite, 20,  0, 10, 10, x, y, sq, sq)
          of '|': ctx.drawImage(sprite,  0, 10, 10, 10, x, y, sq, sq)
          of 'L': ctx.drawImage(sprite,  0, 20, 10, 10, x, y, sq, sq)
          of 'J': ctx.drawImage(sprite, 20, 20, 10, 10, x, y, sq, sq)
          of 'I': ctx.drawImage(sprite, 10, 10, 10, 10, x, y, sq, sq)
          of 'O': ctx.drawImage(sprite, 10, 30, 10, 10, x, y, sq, sq)
          of 'S': ctx.drawImage(sprite, 30, 10, 10, 10, x, y, sq, sq)
          else: ctx.fillRoundedRect(rect(vec2(x.float32, y.float32), vec2(sq.float32, sq.float32)), 2.0)

    image.writeFile("day10.png")

  verifyPart(1, 6907)
  verifyPart(2, 541)
