---
title: Inline constant pool
category: Deltas
order: 2
---

> Fix link
The delta [Inline constant pool]() makes Java bytecode more programmer-friendly by inlining the elements from the constant pool.
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
