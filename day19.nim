import aocd
import std/[strutils, strscans, strformat, sequtils, sets, tables, unittest]

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

check options(1..8) == 8
check options(9..0) == 0
check options(1..1) == 1
check options(6..0) == 0

func options(a: Attributes): int =
  a.cool.options * a.musical.options * a.aero.options * a.shiny.options

func isPossible(s: Slice[int]): bool =
  s.options >= 1

check isPossible(1..8)
check not isPossible(9..0)

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

func eval(workflow: Workflow, part: Part): string =
  for rule in workflow:
    if (let (matches, target) = eval(rule, part); matches):
      return target
  assert false

check @[Rule(operation: Reject, target: "R"), Rule(operation: Accept, target: "A")].eval(Part()) == "R"
check @[Rule(operation: LessCool, value: 42, target: "cool"), Rule(operation: Accept, target: "A")].eval(Part(cool: 3)) == "cool"

func eval(workflows: Workflows, part: Part): bool =
  ## Returns true for parts that are ultimately accepted
  var workflow = workflows["in"]
  while true:
    let target = workflow.eval(part)
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

check (1..8) * (3..7) == 3..7
check (3..7) * (1..8) == 3..7
check (1..8) * (6..15) == 6..8
check (0..0) * (6..3) == 6..0
check not isPossible(6..0)
check not isPossible(1..0)

func `*`(a, b: Attributes): Attributes =
  Attributes(cool: a.cool * b.cool, musical: a.musical * b.musical, aero: a.aero * b.aero, shiny: a.shiny * b.shiny)

func `>=`(a: Slice[int], l: int): Slice[int] =
  result = max(a.a, l+1)..max(a.b, l+1)
  result = result * a
  if not result.isPossible:
    result = 1..0

check (1..8) >= 3 == (4..8)
check (5..8) >= 3 == (5..8)
check (3..7) >= 3 == (4..7)
check (3..7) >= 8 == (1..0)

func `<=`(a: Slice[int], h: int): Slice[int] =
  result = min(a.a, h-1)..min(a.b, h-1)
  result = result * a
  if not result.isPossible:
    result = 1..0

check (1..8) <= 7 == (1..6)
check (3..7) <= 7 == (3..6)
check (1..5) <= 7 == (1..5)
check (3..7) <= 3 == (1..0)
check (3..7) <= 2 == (1..0)

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
            of MoreCool:    cool = cool >= rule.value       ; cat = Cool
            of LessCool:    cool = cool <= rule.value       ; cat = Cool
            of MoreMusical: musical = musical >= rule.value ; cat = Musical
            of LessMusical: musical = musical <= rule.value ; cat = Musical
            of MoreAero:    aero = aero >= rule.value       ; cat = Aero
            of LessAero:    aero = aero <= rule.value       ; cat = Aero
            of MoreShiny:   shiny = shiny >= rule.value     ; cat = Shiny
            of LessShiny:   shiny = shiny <= rule.value     ; cat = Shiny

            frontier.incl RuleNode(label: rule.target, cool: cool, musical: musical, aero: aero, shiny: shiny)

            case rule.operation
            of Reject, Accept, Move: discard
            of MoreCool:
              nextAttributes.add(Attributes(cool: attrs.cool <= rule.value+1, musical: musical, aero: aero, shiny: shiny))
            of LessCool:
              nextAttributes.add(Attributes(cool: attrs.cool >= rule.value-1, musical: musical, aero: aero, shiny: shiny))
            of MoreMusical:
              nextAttributes.add(Attributes(cool: cool, musical: attrs.musical <= rule.value+1, aero: aero, shiny: shiny))
            of LessMusical:
              nextAttributes.add(Attributes(cool: cool, musical: attrs.musical >= rule.value-1, aero: aero, shiny: shiny))
            of MoreAero:
              nextAttributes.add(Attributes(cool: cool, musical: musical, aero: attrs.aero <= rule.value+1, shiny: shiny))
            of LessAero:
              nextAttributes.add(Attributes(cool: cool, musical: musical, aero: attrs.aero >= rule.value-1, shiny: shiny))
            of MoreShiny:
              nextAttributes.add(Attributes(cool: cool, musical: musical, aero: aero, shiny: attrs.shiny <= rule.value+1))
            of LessShiny:
              nextAttributes.add(Attributes(cool: cool, musical: musical, aero: aero, shiny: attrs.shiny >= rule.value-1))

          attributes = nextAttributes

    # TODO: Calculate the *unique* number of possible combinations of attributes
    #  maybe enough to deal with the range boundaries only?
    # result = 0
    # for node in passable:
      # debugEcho fmt"{node.cool.a:4}..{node.cool.b:4} {node.musical.a:4}..{node.musical.b:4} {node.aero.a:4}..{node.aero.b:4} {node.shiny.a:4}..{node.shiny.b:4}"
      # result += node.cool.options * node.musical.options * node.aero.options * node.shiny.options

    result = 0
    var ns = passable.toSeq.mapIt(Attributes(cool: it.cool, musical: it.musical, aero: it.aero, shiny: it.shiny))
    let nsh = ns.high

    # Double check with part 1?
    var party = 0
    for part in parts:
      for node in passable:
        if part.cool in node.cool and
            part.musical in node.musical and
            part.aero in node.aero and
            part.shiny in node.shiny:
          party+= part.cool + part.musical + part.aero + part.shiny
          break
    debugEcho fmt"party: {party}"

    # Include cardinalities of the sets
    # Exclude the cardinalities of the pair-wise intersections
    # Include the cardinalities of the triple-wise intersections
    # Exclude the cardinalities of the quadruple-wise intersections
    # ...etc until we reach the cardinality of the intersection of all sets, or
    # we run out of intersections.

    debugEcho fmt"possible nodes: {ns.len}"
    debugEcho fmt"calculating 0: {result}"

    var before = result
    for i in 0..nsh:
      result += ns[i].options
    if result == before:
      debugEcho "no intersections"
      return result

    debugEcho fmt"calculating 1: {result}"

    before = result
    for i in 0..nsh-1:
      for j in i+1..nsh:
        result -= (ns[i] * ns[j]).options
    if result == before:
      debugEcho "no intersections"
      return result

    debugEcho fmt"calculating 2: {result}"

    before = result
    for i in 0..nsh-2:
      for j in i+1..nsh-1:
        for k in j+1..nsh:
          result += (ns[i] * ns[j] * ns[k]).options
    if result == before:
      debugEcho "no intersections"
      return result

    debugEcho fmt"calculating 3: {result}"

    before = result
    for i in 0..nsh-3:
      for j in i+1..nsh-2:
        for k in j+1..nsh-1:
          for l in k+1..nsh:
            result -= (ns[i] * ns[j] * ns[k] * ns[l]).options
    if result == before:
      debugEcho "no intersections"
      return result

    debugEcho fmt"calculating 4: {result}"

    before = result
    for i in 0..nsh-4:
      for j in i+1..nsh-3:
        for k in j+1..nsh-2:
          for l in k+1..nsh-1:
            for m in l+1..nsh:
              result += (ns[i] * ns[j] * ns[k] * ns[l] * ns[m]).options
    if result == before:
      debugEcho "no intersections"
      return result

    debugEcho fmt"calculating 5: {result}"

    for i in 0..nsh-5:
      for j in i+1..nsh-4:
        for k in j+1..nsh-3:
          for l in k+1..nsh-2:
            for m in l+1..nsh-1:
              for n in m+1..nsh:
                result -= (ns[i] * ns[j] * ns[k] * ns[l] * ns[m] * ns[n]).options

  verifyPart(1, 383682)
  verifyPart(2, 117954800808317)

  # For example input
  # verifyPart(1, 19114)
  # verifyPart(2, 167409079868000)
