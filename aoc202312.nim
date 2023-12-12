import aocd
import std/[math, algorithm, strutils, strformat, sets, sequtils, unittest]

const example1 {.used.} = """
???.###              1,1,3
.??..??...?##.       1,1,3
?#?#?#?#?#?#?#?      1,3,1,6
????.#...#...        4,1,1
????.######..#####.  1,6,5
?###????????         3,2,1
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

type
  State = enum
    Opr, Dmg, Unk

const
  States = {'.', '#', '?'}

func unfold(record: Record): Record =
  let newRecord = [record.record, record.record, record.record, record.record, record.record].join($Unknown)
  let newDamageRuns = cycle(record.damageRuns, 5)
  Record(record: newRecord, damageRuns: newDamageRuns)

iterator fit(record: string, damage: int, startingAt: int, reservedDamage: int): Match =
  let h = min(record.len - damage, record.len - reservedDamage - damage)
  var j = 0
  for i in startingAt..h:
    j = i + damage

    if i - 1 >= 0 and record[i - 1] == Damaged:
      continue
    elif j < record.len and record[j] == Damaged:
      continue

    if record[i..<j].allIt(it == Unknown or it == Damaged):
      let prefix = if i > 0: record[0..<i-1] & $Operational else: ""
      let suffix = if j <= record.high: $Operational & record[j+1..^1] else: ""
      yield Match(record: prefix & repeat(Damaged, damage) & suffix, s: i, l: damage)

proc fitt(record: string, damage: openArray[int]): int =
  let totalDamage = damage.toSeq.foldl(a + b)
  var candidates = initHashSet[Match]()
  var coveredDamage = 0
  var damage = damage.toSeq.reversed

  candidates.incl Match(record: record, s: 0, l: 0)

  while damage.len > 0 and candidates.len > 0:
    # echo fmt"Damage: {damage.len}, candidates: {candidates.card}"
    let d = damage.pop
    var dc: HashSet[Match]
    let remainingDamage = totalDamage - coveredDamage - d

    for mp in candidates:
      for m in fit(mp.record, d, mp.s + mp.l, remainingDamage):
        dc.incl m
    coveredDamage += d
    candidates = dc

  var cleaned = initHashSet[string]()
  for m in candidates:
    if m.record.countIt(it == Damaged) == totalDamage: # and '?' notin candidate:
      cleaned.incl m.record.replace(Unknown, Operational)

  return cleaned.card

proc fittt(record: Record): int =
  fitt(record.record, record.damageRuns)

# check fittt(Record(record: "???.###", damageRuns: @[1, 1, 3]).unfold).toSeq.len == 1
# check fitt(".??..??...?##.", [1, 1, 3]).toSeq.len == 4
# check fitt("?#?#?#?#?#?#?#?", [1, 3, 1, 6]).toSeq.len == 1
# check fitt("????.#...#...", [4, 1, 1]).toSeq.len == 1
# check fitt("????.######..#####.", [1, 6, 5]).toSeq.len == 4
# check fitt("?###????????", [3, 2, 1]).toSeq.len == 10

# for ma in fit("?###????????", 3, 0):
#   echo fmt"{ma} -> "
#   for mb in fit(ma.record, 2, ma.s + ma.l):
#     echo fmt"{ma} -> {mb}"
#     for mc in fit(mb.record, 1, mb.s + mb.l):
#       echo fmt"{ma} -> {mb} -> {mc}"
#       # for de in fit(mc.record, 0, mc.s + mc.l):
#       #   echo fmt"{ma} -> {mb} -> {mc} -> {de}"

# for line in fitt("?###????????", [3, 2, 1]).toSeq.sorted:
#   echo line

day 12:
  var springRecords: seq[Record]
  for line in example1.strip.splitLines:
    let parts = line.splitWhitespace
    let runs = parts[1].split(",").map(parseInt)
    springRecords.add(unfold Record(record: parts[0], damageRuns: runs))

  # Do the difficult ones first
  springRecords = springRecords.sortedByIt:
    -it.record.countIt(it == Unknown)

  var answers = newSeq[int](input.strip.splitLines.toSeq.len)
  let count = springRecords.len
  for r in 0..springRecords.high:
    let m = springRecords[r]
    # echo "Working on " & $springRecords[r]
    answers[r] = fittt(m)
    echo fmt"{r+1}/{count}: {answers[r]}"
  var answer1 = answers.toSeq.foldl(a + b)

  part 1:
    answer1

  part 2:
    0

  verifyPart(1, 7460)
  verifyPart(2, 0)
