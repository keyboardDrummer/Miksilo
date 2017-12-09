---
title: Interactions between delta's
category: Deltas
order: 4
---

In this article we'll look at interactions between independent delta's. We show an example of an interaction that results in a free language feature, and another one leading to a confusing language feature, but one that can be patched to work well using an extra delta.

We'll start by looking at the interaction between the following delta's:

- [WhileLoop](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/WhileLoopDelta.scala), which adds a while loop to the language.
- [WhileBreak](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/WhileBreakDelta.scala), which adds a break statement for while loops.
- [ForLoop](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/ForLoopDelta.scala), which adds a C-style for loop to the language. It depends on the WhileLoop delta because it compiles by transforming the for loop to an equivalent while loop.

If we compose these delta's in the order `[ForLoop, WhileBreak, WhileLoop]`, then we get a language where the break statements works both in the for loop and in the while loop, even though the break was only designed to work with a while loop. Basically we get a feature for free!

Now let's look at the delta [WhileContinue](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/WhileContinueDelta.scala), which introduces the continue statement for while loops.

If we compose this in the order `[ForLoop, WhileContinue, WhileLoop]`, will we then get a free language feature like with `WhileBreak`? At first it seems like it, because the compiler will accept a continue statement inside a for loop. However, let's see what happens if we run the following program:

```java
for(int i = 0; i < 3; i++) {
   if (i == 1)
      continue;
   System.out.print(i);
}
```

We're expecting this program to print 0 and 2, but running it shows that it prints 0 and then just keeps running. What happened? Let's look at the code after running the ForLoop and WhileContinue compilation phases:

```java
int i = 0;
label <whileStart>;
while(i < 3)
{
    if(i==1)
        goto <whileStart>;
    System.out.print(i);
    i++;
}
```

We see that the continue statement has been translated to `goto <whileStart>`, which jumps to `label <whileStart>`. However, this skips over the increment statement `i++`, so our while loop never progresses and our program won't terminate.

To solve this problem, one thing we could do is change the delta ForLoop, so that we put `i++` at the start of the while. Note that this requires other changes as well, which reduce the quality of the ForLoop compilation. Our ideology is that every delta should be self-centered, meaning it should be the best it can be, instead of accommodating for possible future delta's. We decide not to change ForLoop. Luckily with Blender, every problem can be solved with another delta.

The delta [ForLoopContinue](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/statements/ForLoopContinueDelta.scala) depends on both ForLoop and WhileContinue. It solves the above problem by transforming our initial program containing the for loop, so that it becomes:

```java
for(int i = 0; i < 3; i++) {
   if (i == 1)
      goto <beforeIncrement>;
   System.out.print(i);
   label <beforeIncrement>;
}
```

And then after compiling ForLoop:

```java
int i = 0;
while(i < 3)
{
    if(i==1)
        goto <beforeIncrement>;
    System.out.print(i);
    label <beforeIncrement>;
    i++;
}
```

Running this prints both 0 and 2, and does terminate.

For those interested, the WhileLoop is also compiled into simpler statements, producing:

```java
int i = 0;
label <whileStart>
if(i < 3)
{
    if(i==1)
        goto <beforeIncrement>;
    System.out.print(i);
    label <beforeIncrement>;
    i++;
    goto <whileStart>;
}
```


