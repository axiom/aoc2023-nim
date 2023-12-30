import aocd
import std/[strutils, deques, strformat, strscans, sequtils, sets, intsets, tables, unittest]

type
  Op = enum Pass = "=", FlipFlop = "%", Conj = "&"

  Signal = enum Low, High

  Node = ref NodeObj
  NodeObj = object
    case op: Op
    of Pass:     value: Signal
    of FlipFlop: on: bool
    of Conj:     memory: Table[string, Signal]
    label: string
    outputs: seq[Node]
    inputs: int

  Input = object
    sender: string
    target: string
    value: Signal

func allHigh(node: Node): bool =
  case node.op
  of Pass:     false
  of FlipFlop: false
  of Conj:
    for value in node.memory.values:
      if value == Low:
        return false
    return true

func `$`(input: Input): string =
  fmt"{input.sender} -{input.value}-> {input.target}"

func `$`(node: Node): string =
  let outputs = node.outputs.mapIt(it.label).join(", ")
  case node.op
  of Pass:
    fmt"{node.op}{node.label} [{node.value}] -> {outputs}"
  of FlipFlop:
    fmt"{node.op}{node.label} [{node.on}] -> {outputs}"
  of Conj:
    fmt"{node.op}{node.label} [{node.allHigh}/{node.memory}] -> {outputs}"

func eval(node: var Node, input: Input): seq[Input] =
  case node.op
  of Pass:
    node.outputs.mapIt:
      Input(sender: node.label, target: it.label, value: input.value)
  of FlipFlop:
    case input.value
    of Low:
      let output = if node.on: Low else: High
      node.on = not node.on
      node.outputs.mapIt:
        Input(sender: node.label, target: it.label, value: output)
    of High:
      @[]
  of Conj:
    node.memory[input.sender] = input.value
    let signal = if node.allHigh: Low else: High
    node.outputs.mapIt:
      Input(sender: node.label, target: it.label, value: signal)

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
  let puzzle = input
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
        assert false
        ParseNode()

    # Convert parse nodes to real nodes
    var nodes: Table[string, Node]
    for pnode in parseNodes:
      nodes[pnode.label] = Node(op: pnode.op, label: pnode.label)
      # Add all the outputs as nodes also if they not already exists.
      for output in pnode.outputs:
        if output notin nodes:
          nodes[output] = Node(op: Pass, label: output)

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
          output.memory[node.label] = Low
        of FlipFlop: discard
        of Pass: discard

    nodes

  # Print graphviz graph
  for node in nodes.values:
    let color = case node.op
                of Pass: "honeydew"
                of FlipFlop: "mistyrose"
                of Conj: "aliceblue"
    echo fmt"""{node.label} [label="{node.op} {node.label}", fillcolor={color}];"""
  echo ""
  for node in nodes.values:
    for output in node.outputs:
      echo fmt"""{node.label} -> {output.label};"""
  echo ""

  part 1:
    var lows, highs: int
    for b in 1..1000:
      var inputs: Deque[Input]
      inputs.addLast Input(sender: "button", target: "broadcaster", value: Low)

      while inputs.len > 0:
        let input = inputs.popFirst
        # echo input

        case input.value
        of Low: inc lows
        of High: inc highs

        var node = nodes[input.target]
        for inp in eval(node, input):
          inputs.addLast inp
      # echo ""

    echo fmt"lows: {lows}, highs: {highs} -> {lows * highs}"
    result = highs * lows

  part 2:
    result = 0
    var lows, highs: int
    var hits: Table[string, IntSet]
    var mem: Table[string, Signal]

    for b in 1..1_000_000_000:
      inc result
      var inputs: Deque[Input]
      inputs.addLast Input(sender: "button", target: "broadcaster", value: Low)

      while inputs.len > 0:
        let input = inputs.popFirst

        if input.target == "zh" and input.value == High:
          mem[input.sender] = input.value
          echo fmt"b: {b}: {mem}"

        var node = nodes[input.target]
        for inp in eval(node, input):
          inputs.addLast inp

    result = 1
    for hit in hits.values:
      result *= hit.toSeq[0]

  # 686598143 is too low
  # For example input
  # verifyPart(1, 32000000)
  verifyPart(1, 11687500)
  # 75551871864483 is too low
  verifyPart(2, 0)
