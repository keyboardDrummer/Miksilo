---
title: Inline constant pool
category: Deltas
order: 2
---

Sometimes a programmer targeting the Java virtual machine may want to write class files directly, instead of writing some JVM languages and having that language be compiled to Java bytecode. Predefined delta's in Blender allow you to write Java bytecode using a grammar inspired by _javap_, and convert that to a Java classfile.

The constant pool in a class file is useful because it reduces code duplication. However, for a programmer it is awkward because the elements in the pool are referenced by index, which are hard to remember when trying to refer to a constant pool element. To make writing Java bytecode more programmer-friendly, it's better to inline elements from the constant pool, which is what the delta [Inline constant pool](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/bytecode/simpleBytecode/InlineConstantPool.scala) does.

It allows the programmer to write:

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

Instead of:

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

There's two parts to inlining the constant pool:

1. A new compilation phase moves all the constant pool entries in the program to a newly created constant pool, and updates the original locations with references.
1. The grammar has to be changed, so that:
   1. The constant pool is removed
   1. References to the constant pool are replaced by constant pool entries
   1. Constant pool entries always start with stating their type, to disambiguate the grammar. After inlining, the entries are in locations where a specific type is expected, so we can remove the type label from the grammar.

In important input to this Delta is the language variable `constantReferences`, it contains for each class, for each of its fields, what type of constant pool element it references, if any.

Here is the code for part 1:

```scala
  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val constantPool = new ConstantPool()
    program.constantPool = constantPool
    val constantReferences = ByteCodeSkeleton.getRegistry(compilation).constantReferences

    PathRoot(program).visit(afterChildren = extractReferencesInNode)

    def extractReferencesInNode(path: Path): Unit = {
      for {
        references: Map[NodeField, NodeClass] <- constantReferences.get(node.clazz)
        reference <- references
        fieldValue <- path.current.get(reference._1)
      } {
        val index = constantPool.store(fieldValue)
        path.current.data.put(reference._1, index)
      }
    }
  }
```