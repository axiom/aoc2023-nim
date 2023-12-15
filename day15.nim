import aocd
import std/[strutils, strscans, sequtils, tables, unittest]

type
  Op = enum
    Rem
    Add
  Lens = object
    label: string
    focal: range[0..9]
  Step = object
    case op: Op
    of Rem: label: string
    of Add: lens: Lens
    box: range[0..255]

func hash (input: string): range[0..255] =
  var h = 0
  for c in input:
    h += c.ord
    h *= 17
    h = h mod 256
  h

check "HASH".hash == 52

const example {.used.} = """
rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
""".strip

day 15:
  let puzzle = input

  part 1:
    result = 0
    for step in puzzle.strip.split(","):
      result += step.hash

  part 2:
    let steps: seq[Step] = puzzle.strip.split(",").mapIt:
      if (let (ok, label, focal) = it.scanTuple("$w=$i$."); ok):
        Step(op: Add, box: label.hash, lens: Lens(label: label, focal: focal))
      elif (let (ok, label) = it.scanTuple("$w-$."); ok):
        Step(op: Rem, box: label.hash, label: label)
      else:
        assert false
        Step(op: Rem, box: 0, label: it)

    # Build up the HASHMAP
    var boxes: array[256, seq[Lens]]
    for step in steps:
      block buildup:
        case step.op:
          of Rem: # Remove labeled lens
            for i, lens in boxes[step.box].mpairs:
              if lens.label == step.label:
                boxes[step.box].delete(i)
                break buildup

          of Add: # Upsert lens with focal length.
            for i, lens in boxes[step.box].mpairs:
              if lens.label == step.lens.label:
                boxes[step.box][i] = step.lens
                break buildup
            boxes[step.box].add(step.lens)

    # Calculate the focal power
    result = 0
    for b, box in boxes.pairs:
      for l, lens in box.pairs:
        result += (b + 1) * (l + 1) * lens.focal

  verifyPart(1, 515210)
  verifyPart(2, 246762)
