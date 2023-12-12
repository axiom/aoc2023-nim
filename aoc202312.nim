import aocd
import std/[algorithm, strutils, strformat, sets, sequtils, unittest]

const example1 {.used.} = """
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"""

type
  Record = object
    record: string
    damageRuns: seq[int]
  Match = object
    record: string
    s, l: int

const
  Operational = '.'
  Damaged = '#'
  Unknown = '?'

func permutations(r: Record): int =
  return r.record.countIt(it == Unknown)

iterator fit(record: string, damage: int, startingAt: int): Match =
  if damage <= 0:
    yield Match(record: record, s: 0, l: 0)
  else:
    var seen = initHashSet[Match]()
    # Recursively fit damage into every possible spot of record?
    # Try each position.
    for i in startingAt..record.len - damage:
      let j = i + damage

      if i - 1 >= 0 and record[i - 1] == Damaged:
        continue
      if j < record.len and record[j] == Damaged:
        continue

      if record[i..<j].allIt(it == Unknown or it == Damaged):
        let prefix = if i > 0: record[0..<i-1] & $Operational else: ""
        let suffix = if j <= record.high: $Operational & record[j+1..^1] else: ""
        let candidate = prefix & repeat(Damaged, damage) & suffix
        let match = Match(record: candidate, s: i, l: damage)
        if match notin seen:
          seen.incl match
          yield match

iterator fitt(record: string, damage: openArray[int]): string =
  let totalDamage = damage.toSeq.foldl(a + b)
  var candidates = initHashSet[Match]()
  var damage = damage.toSeq.reversed

  candidates.incl Match(record: record, s: 0, l: 0)

  while damage.len > 0 and candidates.len > 0:
    let d = damage.pop
    var dc: HashSet[Match]

    for mp in candidates:
      for m in fit(mp.record, d, mp.s + mp.l):
        dc.incl m
    candidates = dc

  var cleaned = initHashSet[string]()
  for m in candidates:
    if m.record.countIt(it == Damaged) == totalDamage: # and '?' notin candidate:
      cleaned.incl m.record.replace(Unknown, Operational)

  for c in cleaned:
    yield c

check fitt("???.###", [1, 1, 3]).toSeq.len == 1
check fitt(".??..??...?##.", [1, 1, 3]).toSeq.len == 4
check fitt("?#?#?#?#?#?#?#?", [1, 3, 1, 6]).toSeq.len == 1
check fitt("????.#...#...", [4, 1, 1]).toSeq.len == 1
check fitt("????.######..#####.", [1, 6, 5]).toSeq.len == 4
check fitt("?###????????", [3, 2, 1]).toSeq.len == 10

# for ma in fit("?###????????", 3, 0):
#   echo fmt"{ma} -> "
#   for mb in fit(ma.record, 2, ma.s + ma.l):
#     echo fmt"{ma} -> {mb}"
#     for mc in fit(mb.record, 1, mb.s + mb.l):
#       echo fmt"{ma} -> {mb} -> {mc}"
#       # for de in fit(mc.record, 0, mc.s + mc.l):
#       #   echo fmt"{ma} -> {mb} -> {mc} -> {de}"

for line in fitt("?###????????", [3, 2, 1]).toSeq.sorted:
  echo line

day 12:
  var springRecords: seq[Record]
  for line in input.strip.splitLines:
    let parts = line.split(" ")
    let runs = parts[1].split(",").map(parseInt)
    springRecords.add(Record(record: parts[0], damageRuns: runs))

  var sum = 0
  for record in springRecords:
    sum += fitt(record.record, record.damageRuns).toSeq.len
  echo sum

  part 1:
    result = 0
      # let fits = fit(record.record, record.damageRuns).toSeq.len
      # echo fits

  part 2:
    0

  verifyPart(1, 21)
  verifyPart(2, 0)
