---
title: Inline constant pool
category: Deltas
order: 2
---

In some scenario's a programmer might want to write class files directly, instead of writing some JVM languages and having that language be compiled to Java bytecode. Predefined delta's in Blender allow you to write Java bytecode using a grammar inspired by `javap`, and emit a Java classfile.

The constant pool in a class file is useful because it reduces code duplication. However, for a programmer it is hard to use because the elements in the pool are referenced by index, and it's hard to remember which index refers to which constant pool element. To make writing Java bytecode more programmer-friendly, it's better to inline the elements from the constant, which is what the delta [Inline constant pool](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/bytecode/simpleBytecode/InlineConstantPool.scala) does.

It allows the programmer to write this:

```java
class Empty extends java/lang/Object with interfaces: ()
{
  Method;
    name: <init>
    descriptor: ()V
    flags: ACC_PUBLIC
    Code:
      name: Code, stack:1, locals:1
        aload 0
        invokespecial java/lang/Object.<init> ()V
        return
      Exceptions:
}
```

Instead of this:
```java
class 9 extends 5 with interfaces: ()
Constant pool:
  #1 = Utf8 Empty
  #2 = Utf8 java/lang/Object
  #3 = Utf8 <init>
  #4 = Utf8 ()V
  #5 = Class 2
  #6 = NameAndType 3 4
  #7 = Methodref 5.6
  #8 = Utf8 Code
  #9 = Class 1
{
  Method;
    name: 3
    descriptor: 4
    flags: ACC_PUBLIC
    Code:
      name: 8, stack:1, locals:1
        aload 0
        invokespecial 7
        return
      Exceptions:
}

```

References to the constant pool occur throughout the entire class file, so inlining them is quite a big change. Let's look at the source of [Inline constant pool](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/bytecode/simpleBytecode/InlineConstantPool.scala) to see the work involved.