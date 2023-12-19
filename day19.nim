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

  RuleNode = object
    label: string
    cool, musical, aero, shiny: Slice[int]

  Attributes = object
    cool, musical, aero, shiny: Slice[int]

  Workflow = seq[Rule]

  Workflows = Table[string, Workflow]

func options(s: Slice[int]): int =
  max(0, s.b - s.a + 1)

func isPossible(s: Slice[int]): bool =
  s.options >= 1

func isPossible(node: RuleNode): bool =
  node.cool.isPossible or node.musical.isPossible or node.aero.isPossible or node.shiny.isPossible

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

func `*`(a, b: Slice[int]): Slice[int] =
  max(a.a, b.a)..min(a.b, b.b)

func `>=`(a: var Slice[int], l: int) =
  a = max(a.a, l)..max(a.b, l)
  if not a.isPossible:
    a = 9..0

func `<=`(a: var Slice[int], h: int) =
  a = min(a.a, h)..min(a.b, h)
  if not a.isPossible:
    a = 9..0

const example {.used.} = """
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
"""

day 19:
  let inputParts = example.strip.split("\n\n")
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
    # Here we need to figure out all the possible combinations of attributes
    # that will lead to acceptance. Graph-problem, I guess? Form start node,
    # what are all distinct paths that lead to accept, and what are the
    # attribute values that will pass through?
    #
    # Each rule forms an edge to another rule-node, which in extension either
    # leads to the accept node, reject node, or some other rule-node.
    # Can I start with visualizing the graph? How many distinct paths to the
    # accept-node, a single one, or lots of them?
    # Each rule-node accumulates the inverse of the previous rule's condition,
    # otherwise we would not reach that node. Propegate forward the acceptable
    # attribute values while building up a graph?
    var passable: HashSet[RuleNode]
    var frontier: HashSet[RuleNode]
    var seen: HashSet[string]
    frontier.incl RuleNode(label: "in", cool: 1..4000, musical: 1..4000, aero: 1..4000, shiny: 1..4000)
    block outer:
      while frontier.len > 0:
        let node = frontier.pop

        if not node.isPossible:
          continue

        if node.label == "R":
          continue

        if node.label == "A":
          passable.incl node
          continue

        if node.label notin workflows:
          debugEcho fmt"unknown node: {node.label}"
          assert false

        let workflow = workflows[node.label]
        var attributes: seq[Attributes] = @[Attributes(cool: node.cool, musical: node.musical, aero: node.aero, shiny: node.shiny)]

        for rule in workflow:
          var nextAttributes: seq[Attributes]
          while attributes.len > 0:
            let attrs = attributes.pop
            var
              cool = attrs.cool
              musical = attrs.musical
              aero = attrs.aero
              shiny = attrs.shiny

            var cat: Category
            case rule.operation
            of Reject, Accept, Move: discard
            of MoreCool:    cool    >= rule.value; cat = Cool
            of LessCool:    cool    <= rule.value; cat = Cool
            of MoreMusical: musical >= rule.value; cat = Musical
            of LessMusical: musical <= rule.value; cat = Musical
            of MoreAero:    aero    >= rule.value; cat = Aero
            of LessAero:    aero    <= rule.value; cat = Aero
            of MoreShiny:   shiny   >= rule.value; cat = Shiny
            of LessShiny:   shiny   <= rule.value; cat = Shiny

            frontier.incl RuleNode(label: rule.target, cool: cool, musical: musical, aero: aero, shiny: shiny)

            case cat
            of Cool:
              if cool.a > 1: nextAttributes.add(Attributes(cool: 1..cool.a - 1, musical: musical, aero: aero, shiny: shiny))
              if cool.b < 4000: nextAttributes.add(Attributes(cool: cool.b + 1..4000, musical: musical, aero: aero, shiny: shiny))
            of Musical:
              if musical.a > 1: nextAttributes.add(Attributes(cool: cool, musical: 1..musical.a - 1, aero: aero, shiny: shiny))
              if musical.b < 4000: nextAttributes.add(Attributes(cool: cool, musical: musical.b + 1..4000, aero: aero, shiny: shiny))
            of Aero:
              if aero.a > 1: nextAttributes.add(Attributes(cool: cool, musical: musical, aero: 1..aero.a - 1, shiny: shiny))
              if aero.b < 4000: nextAttributes.add(Attributes(cool: cool, musical: musical, aero: aero.b + 1..4000, shiny: shiny))
            of Shiny:
              if shiny.a > 1: nextAttributes.add(Attributes(cool: cool, musical: musical, aero: aero, shiny: 1..shiny.a - 1))
              if shiny.b < 4000: nextAttributes.add(Attributes(cool: cool, musical: musical, aero: aero, shiny: shiny.b + 1..4000))
          attributes = nextAttributes

    result = 0
    # TODO: Calculate the *unique* number of possible combinations of attributes
    #  maybe enough to deal with the range boundaries only?
    for node in passable:
      debugEcho fmt"{node.cool.a:4}-{node.cool.b:4} {node.musical.a:4}-{node.musical.b:4} {node.aero.a:4}-{node.aero.b:4} {node.shiny.a:4}-{node.shiny.b:4}"
      result += node.cool.options * node.musical.options * node.aero.options * node.shiny.options

  # verifyPart(1, 383682)
  # 37214657618763 is too low
  # For example input
  verifyPart(1, 19114)
  verifyPart(2, 167409079868000)
