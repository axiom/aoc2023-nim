import aocd
import std/[strutils, strscans, strformat, sequtils, heapqueue, sets, tables, unittest]

type
  Category = enum
    Cool, Musical, Aero, Shiny

  Part = object
    cool, musical, aero, shiny: int

  Condition = enum
    MoreCool, LessCool, MoreMusical, LessMusical, MoreAero, LessAero, MoreShiny, LessShiny, Move, Reject, Accept

  Rule = object
    operation: Condition
    value: int
    target: string

  Workflow = seq[Rule]

  Workflows = Table[string, Workflow]

func valueOf(part: Part): int =
  part.cool + part.musical + part.aero + part.shiny

func eval(rule: Rule, part: Part): (bool, string) =
  let matches = case rule.operation:
    of Reject, Accept, Move: true
    of MoreCool:    part.cool    > rule.value
    of LessCool:    part.cool    < rule.value
    of MoreMusical: part.musical > rule.value
    of LessMusical: part.musical < rule.value
    of MoreAero:    part.aero    > rule.value
    of LessAero:    part.aero    < rule.value
    of MoreShiny:   part.shiny   > rule.value
    of LessShiny:   part.shiny   < rule.value
  (matches, rule.target)

check Rule(operation: Reject, target: "R").eval(Part()) == (true, "R")
check Rule(operation: Accept, target: "A").eval(Part()) == (true, "A")
check Rule(operation: Move, target: "qzb").eval(Part()) == (true, "qzb")

func eval(workflow: Workflow, part: Part): (bool, string) =
  for rule in workflow:
    let (matches, target) = eval(rule, part)
    if (matches):
      return (true, target)
  assert false

check @[Rule(operation: Reject, target: "R"), Rule(operation: Accept, target: "A")].eval(Part()) == (true, "R")
check @[Rule(operation: LessCool, value: 42, target: "cool"), Rule(operation: Accept, target: "A")].eval(Part(cool: 3)) == (true, "cool")

func eval(workflows: Workflows, part: Part): bool =
  ## Returns true for parts that are ultimately accepted
  var workflow = workflows["in"]
  while true:
    let (matches, target) = workflow.eval(part)
    if target == "A":
      return true
    elif target == "R":
      return false
    elif target in workflows:
      workflow = workflows[target]
    else:
      debugEcho fmt"unknown target: {target}"
      assert false

check eval({"in": @[Rule(operation: Reject, target: "R"), Rule(operation: Accept, target: "A")]}.toTable, Part()) == false
check eval({
  "in": @[
    Rule(operation: Move, target: "x"),
    Rule(operation: Reject, target: "R")],
  "x": @[Rule(operation: Accept, target: "A")]}.toTable, Part()) == true

day 19:
  let inputParts = input.strip.split("\n\n")
  let workflows: Table[string, Workflow] = block:
    var workflows: Table[string, Workflow]
    for workflow in inputParts[0].strip.splitLines:
      let (nameOk, name, rest) = workflow.scanTuple("$w{$+}$.")
      assert nameOk
      let rules = rest.split(",").mapIt:
        if (it == "R"):
          Rule(operation: Reject, target: "R")
        elif (it == "A"):
          Rule(operation: Accept, target: "A")
        elif (let (ok, value, target) = it.scanTuple("a>$i:$w$."); ok):
          Rule(operation: MoreAero, value: value, target: target)
        elif (let (ok, value, target) = it.scanTuple("a<$i:$w$."); ok):
          Rule(operation: LessAero, value: value, target: target)
        elif (let (ok, value, target) = it.scanTuple("m>$i:$w$."); ok):
          Rule(operation: MoreMusical, value: value, target: target)
        elif (let (ok, value, target) = it.scanTuple("m<$i:$w$."); ok):
          Rule(operation: LessMusical, value: value, target: target)
        elif (let (ok, value, target) = it.scanTuple("x>$i:$w$."); ok):
          Rule(operation: MoreCool, value: value, target: target)
        elif (let (ok, value, target) = it.scanTuple("x<$i:$w$."); ok):
          Rule(operation: LessCool, value: value, target: target)
        elif (let (ok, value, target) = it.scanTuple("s>$i:$w$."); ok):
          Rule(operation: MoreShiny, value: value, target: target)
        elif (let (ok, value, target) = it.scanTuple("s<$i:$w$."); ok):
          Rule(operation: LessShiny, value: value, target: target)
        elif (let (ok, target) = it.scanTuple("$w$."); ok):
          Rule(operation: Move, target: target)
        else:
          debugEcho fmt"unknown rule: {it}"
          assert false
          Rule(operation: Reject)
      workflows[name] = rules
    workflows
  let parts: seq[Part] = inputParts[1].strip.splitLines.mapIt:
    let (ok, cool, musical, aero, shiny) = it.scanTuple("{x=$i,m=$i,a=$i,s=$i}$.")
    assert ok
    Part(cool: cool, musical: musical, aero: aero, shiny: shiny)

  part 1:
    let accepted = parts.filterIt(workflows.eval(it))
    return accepted.mapIt(it.valueOf).foldl(a + b)

  part 2:
    0

  verifyPart(1, 383682)
