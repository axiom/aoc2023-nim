import aocd
import std/[strutils, sequtils, strformat, tables, unittest]

type
  Condition = enum
    Dmg = "#"
    Ope = "."
    Unk = "?"
  Record = object
    conditions: seq[Condition]
    groups: seq[int]
  CacheKey = object
    c: int ## How far into the conditions we are
    g: int ## Which group we are dealing with
    gi: int ## How far into the current group we are

func `$`(c: seq[Condition]): string =
  c.mapIt($it).join

func `$`(r: Record): string =
  result = fmt"{r.conditions.mapIt($it).join} {r.groups}"

var cache: Table[CacheKey, int]

proc solve(arrangement: seq[Condition], r: Record, c, g, gi: int): int =
  ## c is our position in the condition
  ## g is the group we are working on
  ## gi is how many damaged springs that remain to match in the current group
  ##  int.low means that we are currently not matching a group
  ##  0 means that we just matched the last spring of a group, so we must end properly
  let key = CacheKey(c: c, g: g, gi: gi)
  if key in cache:
    echo fmt" cache hit {key}: {cache[key]}"
    return cache[key]

  if gi != int.low or c == r.conditions.high:
    echo $arrangement & fmt" {c:2} {g} {gi}"

  template remret(result: untyped): untyped =
    # let value = result
    # cache[key] = value
    # return value
    return result

  func useAs(cond: Condition): seq[Condition] =
    if c == 0:
      return cond & arrangement[1..arrangement.high]
    elif c == arrangement.high:
      return arrangement[0..c - 1] & cond
    else:
      return arrangement[0..c - 1] & cond & arrangement[c + 1..arrangement.high]

  # let cc = if c > r.conditions.high: "X" else: $r.conditions[c]
  # let cgi = if gi == int.low: "free" else: $gi
  # echo fmt" {cc} solve({c}, {g}, {cgi})"
  # echo $r.conditions[c..r.conditions.high] & " " & $gi

  if g > r.groups.high:
    # We have handled all groups, hopefully there are no unaccounted ? left...
    remret 1

  # Check if the condition is exhausted, and then we better be done with all the
  # matching for us to be in a valid state.
  if c > r.conditions.high:
    if g < r.groups.high or (g == r.groups.high and gi == int.low):
      # There are groups left to match, but no more condition, this is not part
      # of a solution.
      # echo "  groups left, or part of group left"
      remret 0
    elif g == r.groups.high and gi == 0:
      # echo "  ended exactly on the last group!"
      remret 1
    elif g > r.groups.high and gi == int.low:
      # echo "  ended with no more groups left to process!"
      remret 1
    elif g == r.groups.high and gi > 0:
      # We were in the middle of matching a group but ran out of conditions,
      # this is not a solution.
      # echo "  not enough condition left to match"
      remret 0
    else:
      echo fmt"  uncovered case!!! {g} {gi}"
      assert false

  let
    cond = r.conditions[c]
    group = r.groups[g]
    free = gi == int.low
    ended = gi == 0
    inside = gi > 0

  if cond == Unk and free:
    remret solve(useAs Dmg, r, succ c, g, group - 1) + solve(useAs Ope, r, succ c, g, int.low)
  if cond == Unk and inside:
    remret solve(useAs Dmg, r, succ c, g, pred gi)
  if cond == Unk and ended:
    remret solve(useAs Ope, r, succ c, succ g, int.low)

  if cond == Ope and free:
    remret solve(useAs Ope, r, succ c, g, int.low)
  if cond == Ope and inside:
    remret 0
  if cond == Ope and ended:
    remret solve(useAs Ope, r, succ c, succ g, if g == r.groups.high: int.low else: r.groups[succ g])

  if cond == Dmg and free:
    remret solve(useAs Dmg, r, succ c, g, group - 1)
  if cond == Dmg and inside:
    remret solve(useAs Dmg, r, succ c, g, pred gi)
  if cond == Dmg and ended:
    remret 0

  assert false, "unreachable"

proc solve(r: Record): int =
  echo ""
  echo "Solving"
  for x in 0..r.conditions.high:
    if x >= 10:
      stdout.write (x div 10)
    else:
      stdout.write " "
  echo ""
  for x in 0..r.conditions.high:
    stdout.write (x mod 10)
  echo ""

  echo r
  clear cache
  solve(r.conditions, r, 0, 0, int.low)

const CharMap = {'.': Ope, '#': Dmg, '?': Unk}.toTable

func toRecord(conditions: string, groups: string): Record =
  result = Record(
    conditions: conditions.strip.mapIt(CharMap[it]),
    groups: groups.split(',').map(parseInt))

check solve(toRecord("#", "1")) == 1
check solve(toRecord("?", "1")) == 1
check solve(toRecord(".", "1")) == 0
check solve(toRecord("??", "1")) == 2
check solve(toRecord("?.?", "1,1")) == 1
check solve(toRecord("?..", "1")) == 1
check solve(toRecord("???.###", "1,1,3")) == 1
check solve(toRecord(".??..??...?##.", "1,1,3")) == 4

const example {.used.} = """
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"""

day 12:
  let records = example.strip.splitLines.mapIt:
    let parts = it.splitWhitespace(2)
    toRecord(parts[0], parts[1])

  echo fmt"Parsed {records.len} records"

  part 1:
    result = 0
    # for record in records:
    #   result += solve(record)

  part 2:
    0

  # verifyPart(1, 7460)
  # Example
  # verifyPart(1, 21)
  # verifyPart(2, 525152)
