import aocd
import std/[strutils, strscans, sequtils, strformat, tables, unittest]

type
  Op = enum
    Rem
    Add
  Lens = object
    label: string
    focal: int
  Step = object
    op: Op
    box: int
    lens: Lens

const example {.used.} = """
rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
""".strip

func hash (input: string): int =
  result = 0
  for c in input:
    result += c.ord
    result *= 17
    result = result mod 256

check "HASH".hash == 52

day 15:
  # Focal length 1..9
  # initial sequence letters indicates (so just hash the letters to get the lens
  # number?) "label of the lens" to operate?
  # Characters following the label indicates what operation to perform, either =
  # or -
  # - means remove the lens in that box, and then fill boxes forward to glose the gap just caused
  # = -> if if there is a lens with the same label, replace the old lens with the new???
  let puzzle = input

  part 1:
    let sequence = puzzle.strip.split(",")
    result = 0
    for step in sequence:
      result += step.hash

  part 2:
    var boxes: array[256, seq[Lens]]
    let steps: seq[Step] = puzzle.strip.split(",").mapIt:
      if (let (ok, label, focal) = it.scanTuple("$w=$i$."); ok):
        Step(op: Add, box: label.hash, lens: Lens(label: label, focal: focal))
      elif (let (ok, label) = it.scanTuple("$w-$."); ok):
        Step(op: Rem, box: label.hash, lens: Lens(label: label, focal: -1))
      else:
        assert false
        Step(op: Rem, box: 0, lens: Lens(label: it, focal: 0))

    for step in steps:
      case step.op:
        of Rem:
          # Remove any lens with the same label.
          for i, lens in boxes[step.box].mpairs:
            if lens.label == step.lens.label:
              boxes[step.box].delete(i)
              break
        of Add:
          # Add or update the lens with this specific focal length.
          var done = false
          for i, lens in boxes[step.box].mpairs:
            if lens.label == step.lens.label:
              boxes[step.box][i] = step.lens
              done = true
              break
          if not done:
            boxes[step.box].add(step.lens)

    result = 0
    for i, box in boxes.pairs:
      for l, lens in box.pairs:
        result += (i + 1) * (l + 1) * lens.focal

  verifyPart(1, 515210)
  verifyPart(2, 246762)
