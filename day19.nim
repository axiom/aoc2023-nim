import aocd
import std/[strutils, strscans, sequtils, sets, tables, unittest]

type
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

func combinations(a: Attributes): int =
  a.cool.len * a.musical.len * a.aero.len * a.shiny.len

func isPossible(s: Slice[int]): bool =
  s.len >= 1

func isPossible(node: RuleNode): bool =
  node.cool.isPossible or node.musical.isPossible or node.aero.isPossible or node.shiny.isPossible

func `*`(a, b: Slice[int]): Slice[int] =
  max(a.a, b.a)..min(a.b, b.b)

func `*`(a, b: Attributes): Attributes =
  Attributes(cool: a.cool * b.cool, musical: a.musical * b.musical, aero: a.aero * b.aero, shiny: a.shiny * b.shiny)

func `>=`(a: Slice[int], l: int): Slice[int] =
  result = max(a.a, l+1)..max(a.b, l+1)
  result = result * a
  if not result.isPossible:
    result = 1..0

func `<=`(a: Slice[int], h: int): Slice[int] =
  result = min(a.a, h-1)..min(a.b, h-1)
  result = result * a
  if not result.isPossible:
    result = 1..0

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
  ## Parse the puzzle
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
        elif (let (ok, target) = it.scanTuple("$w$."); ok):
          Rule(operation: Move, target: target)
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
        else:
          assert false
          Rule(operation: Reject)
      workflows[name] = rules
    workflows
  let parts: seq[Part] = inputParts[1].strip.splitLines.mapIt:
    let (ok, cool, musical, aero, shiny) = it.scanTuple("{x=$i,m=$i,a=$i,s=$i}$.")
    assert ok
    Part(cool: cool, musical: musical, aero: aero, shiny: shiny)

  ## Process the puzzle
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

  # Seed the frontier with the input node.
  frontier.incl RuleNode(label: "in", cool: 1..4000, musical: 1..4000, aero: 1..4000, shiny: 1..4000)

  block outer:
    while frontier.len > 0:
      let node = frontier.pop

      if not node.isPossible: continue
      if node.label == "R": continue
      if node.label == "A":
        passable.incl node
        continue

      var attributes: seq[Attributes] = @[Attributes(cool: node.cool, musical: node.musical, aero: node.aero, shiny: node.shiny)]

      for rule in workflows[node.label]:
        var nextRuleAttributes: seq[Attributes]
        while attributes.len > 0:
          let attrs = attributes.pop
          var
            cool = attrs.cool
            musical = attrs.musical
            aero = attrs.aero
            shiny = attrs.shiny

          case rule.operation
          of Reject, Accept, Move: discard
          of MoreCool:    cool    = cool    >= rule.value
          of LessCool:    cool    = cool    <= rule.value
          of MoreMusical: musical = musical >= rule.value
          of LessMusical: musical = musical <= rule.value
          of MoreAero:    aero    = aero    >= rule.value
          of LessAero:    aero    = aero    <= rule.value
          of MoreShiny:   shiny   = shiny   >= rule.value
          of LessShiny:   shiny   = shiny   <= rule.value

          frontier.incl RuleNode(label: rule.target, cool: cool, musical: musical, aero: aero, shiny: shiny)

          case rule.operation
          of Reject, Accept, Move: discard
          of MoreCool:    cool    = attrs.cool    <= rule.value+1
          of LessCool:    cool    = attrs.cool    >= rule.value-1
          of MoreMusical: musical = attrs.musical <= rule.value+1
          of LessMusical: musical = attrs.musical >= rule.value-1
          of MoreAero:    aero    = attrs.aero    <= rule.value+1
          of LessAero:    aero    = attrs.aero    >= rule.value-1
          of MoreShiny:   shiny   = attrs.shiny   <= rule.value+1
          of LessShiny:   shiny   = attrs.shiny   >= rule.value-1

          nextRuleAttributes.add(Attributes(cool: cool, musical: musical, aero: aero, shiny: shiny))

        attributes = nextRuleAttributes
  var ns = passable.toSeq.mapIt(Attributes(cool: it.cool, musical: it.musical, aero: it.aero, shiny: it.shiny))

  part 1:
    result = 0
    for part in parts:
      for node in passable:
        if part.cool in node.cool and
            part.musical in node.musical and
            part.aero in node.aero and
            part.shiny in node.shiny:
          result += part.cool + part.musical + part.aero + part.shiny
          break

  part 2:
    result = 0

    # Include cardinalities of the sets
    # Exclude the cardinalities of the pair-wise intersections
    # Include the cardinalities of the triple-wise intersections
    # Exclude the cardinalities of the quadruple-wise intersections
    # ...etc until we reach the cardinality of the intersection of all sets, or
    # we run out of intersections.

    let nsh = ns.high
    var before = result
    for i in 0..nsh:
      result += ns[i].combinations

    before = result
    for i in 0..nsh-1:
      for j in i+1..nsh:
        result -= (ns[i] * ns[j]).combinations

    # Did not need any more rounds than this...

  verifyPart(1, 383682)
  verifyPart(2, 117954800808317)

  # For example input
  # verifyPart(1, 19114)
  # verifyPart(2, 167409079868000)
