import aocd
import std/[strutils, deques, strformat, strscans, sequtils, sets, tables, unittest]

type
  Op = enum Pass = "", FlipFlop = "%", Conj = "&"

  Signal = enum Low, High

  Node = ref NodeObj
  NodeObj = object
    case op: Op
    of Pass:     value: Signal
    of FlipFlop: on: bool
    of Conj:     highs: Table[string, Signal]
    label: string
    outputs: seq[Node]
    inputs: int

  Input = object
    sender: string
    label: string
    value: Signal

func allHigh(node: Node): bool =
  case node.op
  of Pass:     false
  of FlipFlop: false
  of Conj:
    for value in node.highs.values:
      if value == Low:
        return false
    return true

func `$`(input: Input): string =
  fmt"{input.sender} -{input.value}-> {input.label}"

func `$`(node: Node): string =
  let outputs = node.outputs.mapIt(it.label).join(", ")
  case node.op
  of Pass:
    fmt"{node.op}{node.label} [{node.value}] -> {outputs}"
  of FlipFlop:
    fmt"{node.op}{node.label} [{node.on}] -> {outputs}"
  of Conj:
    fmt"{node.op}{node.label} [{node.allHigh}/{node.highs}] -> {outputs}"

func eval(node: var Node, input: Input): seq[Input] =
  case node.op
  of Pass:
    # Just pass the input along to the output
    node.outputs.mapIt:
      Input(sender: node.label, label: it.label, value: input.value)
  of FlipFlop:
    if input.value == Low:
      let output = if node.on: Low else: High
      node.on = not node.on
      node.outputs.mapIt:
        Input(sender: node.label, label: it.label, value: output)
    else:
      @[]
  of Conj:
    node.highs[input.sender] = input.value
    if node.allHigh:
      node.outputs.mapIt:
        Input(sender: node.label, label: it.label, value: Low)
    else:
      node.outputs.mapIt:
        Input(sender: node.label, label: it.label, value: High)

const example1 {.used.} = """
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
"""

const example2 {.used.} = """
broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output

"""

day 20:
  let puzzle = example2
  let nodes = block:
    type ParseNode = object
      op: Op
      label: string
      outputs: seq[string]
    let parseNodes = puzzle.strip.splitLines.mapIt:
      if (let (ok, left, right) = it.scanTuple("$w -> $+$."); ok):
        ParseNode(op: Pass, label: left, outputs: right.split(", "))
      elif (let (ok, left, right) = it.scanTuple("%$w -> $+$."); ok):
        ParseNode(op: FlipFlop, label: left, outputs: right.split(", "))
      elif (let (ok, left, right) = it.scanTuple("&$w -> $+$."); ok):
        ParseNode(op: Conj, label: left, outputs: right.split(", "))
      else:
        ParseNode()

    # Convert parse nodes to real nodes
    var nodes: Table[string, Node]
    for pnode in parseNodes:
      nodes[pnode.label] = Node(op: pnode.op, label: pnode.label)

    # Wire up the outputs of the nodes
    for pnode in parseNodes:
      nodes[pnode.label].outputs = pnode.outputs.filterIt(it in nodes).mapIt:
        nodes[it]

    # Count the inputs for each node
    for node in nodes.values:
      for output in node.outputs:
        output.inputs += 1
        case output.op
        of Conj:
          output.highs[node.label] = Low
        of FlipFlop: discard
        of Pass: discard

    nodes

  part 1:
    var lows, highs: int
    for b in 1..1:
      var inputs: Deque[Input]
      inputs.addLast Input(sender: "button", label: "broadcaster", value: Low)

      while inputs.len > 0:
        let input = inputs.popFirst
        echo input

        case input.value
        of Low: inc lows
        of High: inc highs

        var node = nodes[input.label]
        for inp in eval(node, input):
          inputs.addLast inp
      echo ""

    echo fmt"lows: {lows}, highs: {highs} -> {lows * highs}"
    result = highs * lows

  part 2:
    result = 0

  # 686598143 is too low
  # For example input
  verifyPart(1, 32000000)
  verifyPart(2, 0)
