import std/[
  re,
  appdirs,
  dirs,
  files,
  httpclient,
  monotimes,
  paths,
  strformat,
  strscans,
  strutils,
  sequtils,
  tables,
  times,
  unittest,
]

# Advent of Code (aoc) helper library for Nim, copied from and inspired by
# multiple other people:
# - https://github.com/MichalMarsalek/Advent-of-code/blob/master/2022/Nim/aoc_logic.nim
# - https://github.com/amb/aoc-nim/blob/master/aoc.nim

#region Utilities for getting the AoC input

proc p(path: string): Path = Path(path)
proc `$`(path: Path): string = path.string
proc `/`(path: Path, sub: string): Path = path / Path(sub)

proc readFile(path: Path): string = readFile($path)
proc writeFile(path: Path, content: string) = writeFile($path, content)

proc recursiveWriteFile*(path: Path, content: string) =
  ## Same as `syncio.writeFile`, but will recursively create directores up to `path`
  ## if they do not exist. Will not error if a file already exists at `path`.
  createDir(path.parentDir)
  path.writeFile(content)

proc getCookie(): string =
  ## Reads the user's AoC session cookie. Location is decided by `getConfigDir()`.
  let cookiePath = getConfigDir() / "aocd-nim" / "session"

  # Create a file if it doesn't exist yet, so the user can quickly paste their
  # session cookie there.
  if not fileExists cookiePath:
    cookiePath.recursiveWriteFile("")

  result = readFile(cookiePath).strip()

  if result == "":
    raise newException(IOError, fmt"Please write your AoC cookie to '{cookiePath}'.")

  var token: string
  if not result.scanf("session=$+", token):
    raise newException(
      ValueError,
      fmt"Session token should be of format 'session=128_CHAR_HEX_NUMBER', " &
      fmt"but got '{result}' instead."
    )

  if token.len != 128:
    raise newException(
      ValueError,
      fmt"Session token should be a 128 character long hexadecimal number, " &
      fmt"but got '{token}' which is {token.len} characters long."
    )

  try:
    discard parseHexInt(token)
  except ValueError:
    raise newException(
      ValueError,
      fmt"Session token should be a hexadecimal number, " &
      fmt"but could not parse' {token}' as a hexadecimal."
    )

const Year = 2023

proc getInput(year, day: int): string =
  ## Fetches the users input for a given year and day.
  ## Will cache the input after the first time this proc is called.
  let cachedInputPath = getCacheDir(p"aocd-nim") / $year / fmt"{day}.txt"
  if fileExists cachedInputPath:
    return readFile(cachedInputPath)

  echo fmt"Downloading input for year {year}, day {day}."
  let client = newHttpClient()
  defer: client.close()
  client.headers["cookie"] = getCookie()

  result = client.getContent(fmt"https://adventofcode.com/{year}/day/{day}/input")
  cachedInputPath.recursiveWriteFile(result)

#endregion

proc printResults(day: int, answers: OrderedTable[int, string], time: Duration) =
  echo "Day " & $day
  for partNum, answer in answers.pairs:
    echo fmt" Part {partNum}: {answer}"
  echo fmt" Time: {time}"

template timed(code: untyped): Duration =
  block:
    let start = getMonoTime()
    code
    let finish = getMonoTime()
    (finish - start)

template day*(day: int, solution: untyped): untyped =
  let input = getInput(Year, day)
  var answers: OrderedTable[int, string]
  var checks: OrderedTable[int, proc (): void]

  template verifyPart(partNum: int, expected: untyped): untyped =
    checks[partNum] = proc (): void =
      let actual = answers[partNum]
      let wanted = $expected
      test "Part " & $partNum & " should be " & wanted:
        check actual == wanted

  let time = timed:
    # Prepare some common input formats
    let input {.inject, used.} = input.strip
    let inputLines {.inject, used.}: seq[string] = input.splitLines
    proc inputInts(): seq[seq[int]] {.inject, used.} = input.splitLines.mapIt(it.findAll(re"\d+").map(parseInt))

    template part(partNum: int, answer: untyped): untyped =
      block:
        # Put the code stuff into a function so that I can evaluate it when I
        # want, and also use the implicit result variable.
        var x = proc (): string =
          proc inner(): auto =
            answer
          return $inner()
        answers[partNum] = x()

    solution

  printResults(day, answers, time)

  # Run any checks to verify that we have the expected output,
  # mostly useful for fixing the expected output of an example while tweaking
  # the solution.
  for expected in checks.values:
    expected()

#region Extra utility functions

proc groupBy*[T](sequence: openArray[T], by: proc (t: T): bool): seq[seq[T]] =
  var groups: seq[seq[T]] = @[]
  var currentGroup: seq[T] = @[]

  for item in sequence:
    if not by(item):
      # Add item to the current group
      currentGroup.add(item)
    else:
      # Encounter a new group, finalize the current group if it's not empty
      if currentGroup.len > 0:
        groups.add(currentGroup)
        currentGroup = @[]  # Start a new group

  # Add the last group if it's not empty
  if currentGroup.len > 0:
    groups.add(currentGroup)

  return groups

func groupByBlank*(strings: seq[string]): seq[seq[string]] =
  var groups: seq[seq[string]] = @[]
  var currentGroup: seq[string] = @[]

  for s in strings:
    if s.len > 0:
      # Add non-blank string to the current group
      currentGroup.add(s)
    else:
      # Encounter a blank string, finalize the current group if it's not empty
      if currentGroup.len > 0:
        groups.add(currentGroup)
        currentGroup = @[]  # Start a new group

  # Add the last group if it's not empty
  if currentGroup.len > 0:
    groups.add(currentGroup)

  return groups

func groupedInts*(input: string): seq[seq[int]] =
  input.splitLines.groupByBlank.mapIt(it.mapIt(parseInt(it)))

func lastIndex*[T: openArray](sequence: T): int =
  sequence.len - 1

func last*[T](sequence: openArray[T]): T = sequence[sequence.len - 1]

func first*[T](sequence: openArray[T]): T = sequence[0]

#endregion
