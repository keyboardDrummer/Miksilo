---
title: Labelled locations
category: Deltas
order: 3
---

In the article [Inline constant pool](http://keyboarddrummer.github.io/Miksilo/deltas/inline-constant-pool), we showed a delta to make programming Java bytecode easier. In this article, we'll take another step towards that end. In bytecode, there are no structured programming statements, instead there are jump instructions. These instructions take an integer as a argument, that specifies an offset in number of bytes, from the jump instruction to another instruction further along the list of instructions. Here is an example of a Code attribute that contains a list of instructions and a StackMapTable:

```
Code:
  Instructions:
    iload 0
    iconst 2
    if_icmpge 7
    iconst 1
    goto 16
    iload 0
    iconst 1
    isub
    invokestatic Fibonacci.fibonacci (I)I
    iload 0
    iconst 2
    isub
    invokestatic Fibonacci.fibonacci (I)I
    iadd
    ireturn
  StackMapTable:
    same frame, offset:9
    same locals, 1 stack item, offset:12
      int
```

The instruction `if_icmpge 7`, takes two values from the top of the stack, compares them, and if the first is greater than the second, it will jump 7 bytes ahead in the instruction list. The jump instruction itself is 3 bytes, the `iconst 1` instruction after it is 1 byte, and the `goto 16` instruction is again 3 bytes. That makes a total of 7 bytes, so the jump will be to the next instruction, `iload 0`.

 Note also that below the instruction the StackMapTable is specified. It must contain an entry for each instruction that is jumped to, in order to tell the JVM what the stack and local variables look like at that point. Each entry has an offset property that specifies to which instruction the entry applies. The first entry has offset 9, counted from the start of the program, which points to the `iload 0` instruction that `if_icmge 7` jumps to. The second entry has offset 12, counted from the previous offset 9, which points to the `ireturn` instruction at the end, which is jumped to be the `goto 16` instruction.

 Understanding the above code by calculating to what instruction each jump goes, and for what instruction each stack map table entry is, is cumbersome. Let's remove all the byte offsets with the delta [LabelledLocations](https://github.com/keyboardDrummer/Miksilo/blob/master/src/main/scala/deltas/bytecode/additions/LabelledLocations.scala). Our example from earlier now becomes:

```
Code:
  Instructions:
    iload 0
    iconst 2
    if_icmpge <false>
    iconst 1
    goto <end>
    label <false>
      same frame
    iload 0
    iconst 1
    isub
    invokestatic Fibonacci.fibonacci (I)I
    iload 0
    iconst 2
    isub
    invokestatic Fibonacci.fibonacci (I)I
    iadd
    label <end>
      same locals, 1 stack item
        int
    ireturn
```

As you can see we've added a new type of instruction, the label, which has a name and contains an entry from the stack map table. The stack map table itself is gone. Also, the jump instructions now point to labels instead of using byte offsets.