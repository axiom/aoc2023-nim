import aocd
import std/[strutils, sequtils, strformat, tables, unittest]

type
  Record = object
    conditions: string
    groups: seq[int]
  CacheKey = object
    c: int ## How far into the conditions we are
    g: int ## Which group we are dealing with
    gi: int ## How far into the current group we are

const
  Ope = '.'
  Dmg = '#'
  Unk = '?'

proc possibleConfigurations(r: Record): int =
  var cache = initTable[CacheKey, int]()

  template remret(key: CacheKey, answer: untyped): untyped =
    ## Remember answer and return it
    cache[key] = answer
    return answer

  proc solve(c, g, gi: int): int =
    let key = CacheKey(c: c, g: g, gi: gi)

    if c > r.conditions.high or g > r.groups.high:
      remret key, 0

    let
      subconds = r.conditions[c..r.conditions.high]
      cond = subconds[0] # The current record state, either '#' or '.'
      next = if subconds.len == 1: Ope else: subconds[1] # The next record state, or pretend next is fine.
      group = r.groups[g]
      remaining = group - gi

    echo fmt"key={key} subconds={subconds} group={group} remaining={remaining}"

    if key in cache:
      echo fmt"  cache hit: {cache[key]}"
      return cache[key]


    if remaining <= 0:
      # The group must end on an operational spring.
      if next == Ope or next == Unk:
        # Skip the next state, since that must be an operational spring, and we
        # don't want to try to start a group there since it will not work.
        echo fmt"  group ended, continue with next group"
        remret key, 1 + solve(c + 2, g + 1, 0)
      else:
        # We could not end the group here, skip to next potentially possible
        # starting point.
        echo fmt"  group could not end, starting over at next cond"
        remret key, 0 #solve(c + 1, g, 0)


    # Now check if we can fit the current group into this place in the
    # condition.
    case cond:
      of Dmg:
        # Wa have manage to place a damage spring at the current condition, so
        # move on to the rest of the condition and the remaining damage
        # springs in the group.
        echo fmt"  damage spring placed, continue with rest of condition and group"
        remret key, solve(c + 1, g, gi + 1)
      of Unk:
        # The state is unknown, it can be either operational or damage, we need
        # to account for both scenarios.
        echo fmt"  unknown state, try both operational and damage"
        remret key, solve(c + 1, g, gi + 1) + solve(c + 1, g, 0)
      of Ope:
        echo fmt"  operational spring blocking, skip and start over"
        remret key, solve(c + 1, g, 0)
      else:
        assert false

  # Fire off the calculations!
  solve(0, 0, 0)

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
    let
      parts = it.splitWhitespace(2)
      conditions = parts[0].strip
      groups = parts[1].split(',').map(parseInt)
    Record(conditions: conditions, groups: groups)

  echo fmt"Parsed {records.len} records"

  part 1:
    result = 0
    for record in records:
      # echo record
      # echo fmt"  {possibleConfigurations(record)}"
      # break
      result += possibleConfigurations(record)

  part 2:
    0

  verifyPart(1, 7460)
