---
title: Inline constant pool
category: Deltas
order: 2
---

Sometimes a programmer targeting the [Java Virtual Machine](https://en.wikipedia.org/wiki/Java_virtual_machine) may want to write [class files](https://en.wikipedia.org/wiki/Java_class_file) directly, instead of writing in some JVM language and compiling that. A simple reason for this could be to get better performance. Predefined deltas in Miksilo allow you to write Java bytecode using a grammar inspired by _javap_, and convert that to a Java classfile. An existing project that does this is [Jasmin](https://en.wikipedia.org/wiki/Jasmin_(software)).

The constant pool in a class file is useful because it reduces code duplication. However, for a programmer it is awkward because the elements in the pool are referenced by index, which are hard to remember when trying to refer to a constant pool element. To make writing Java bytecode more programmer-friendly, it's better to inline elements from the constant pool, which is what the delta [Inline constant pool](https://github.com/keyboardDrummer/Miksilo/blob/master/src/main/scala/deltas/bytecode/simpleBytecode/InlineConstantPool.scala) does.

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

Note that the bytecode here is simply what is generated when compiling an empty Java class. The method is the default constructor that calls the constructor of `Object`, the type from which an empty class implicitly extends.

References to the constant pool occur throughout the entire class file, so inlining them is quite a big change. Let's go through [the source](https://github.com/keyboardDrummer/Miksilo/blob/master/src/main/scala/deltas/bytecode/simpleBytecode/InlineConstantPool.scala) to understand the work involved. We can split the work into the following tasks:

1. Add a compilation phase that moves all the constant pool entries in the program to a newly created constant pool, and updates the original locations with references.
1. Change the grammar, so that:
   1. The constant pool is removed
   1. References to the constant pool are replaced by constant pool entries
   1. Constant pool entries always start with stating their type, to disambiguate the grammar. After inlining, the entries are in locations where a specific type is expected, so we can remove the type label from the grammar.

An important input for this Delta is the language variable `constantReferences`. It contains for each class, for each field of that class, the type of constant pool element that field references, if any. `constantReferences` is constructed by the different deltas that create the elements of bytecode.

Now we're ready to look at some source. Here is the code for the previously mentioned task 1:

```scala
override def transformProgram(program: Node, compilation: Compilation): Unit = {
  val constantPool = new ConstantPool()
  program.constantPool = constantPool
  val fieldConstantTypesPerClass = ByteCodeSkeleton.getRegistry(compilation).constantReferences

  program.visit(afterChildren = extractReferencesInNode)

  def extractReferencesInNode(node: Node): Unit = {
    for {
      fieldConstantTypes <- fieldConstantTypesPerClass.get(node.shape)
      field <- fieldConstantTypes.keys
      constantPoolElement <- node.get(field)
    } {
      val index = constantPool.store(constantPoolElement)
      node.data.put(field, index)
    }
  }
}
```

You can see that we start out by creating a constant pool and storing it in the program, then retrieving the `constantReferences` language variable. We use `visit` to traverse over the entire program, and in `extractReferencesInNode` we detect what constant pool elements a node contains, store those in the constant pool, and replace the original element with a constant pool reference.

For task 2.1, removing the constant pool grammar, we get the code:

```scala
def removeConstantPoolGrammar(language: Language): Unit = {
  val root: Labelled = language.grammars.root
  val constantPoolGrammar: GrammarReference = root.findLabelled(ConstantPoolGrammar)
  constantPoolGrammar.removeMe()
}
```

Here the method `findLabelled` finds the shortest path from the root grammar to the `ConstantPoolGrammar`, and returns that path. The `GrammarReference` type is like a zipper, it describes a path through the grammar tree from a particular root, and has a powerful methods such as `removeMe`, which removes the current grammar from its container.

For task 2.2, which was to replace all constant pool references in the grammar with constant pool elements of the correct type, the source is:

```scala
private def inlineConstantPoolReferences(language: Language): Unit = {
  import language.grammars._
  val constantReferences = ByteCodeSkeleton.getRegistry(language).constantReferences
  val constantPoolIndexGrammar = find(ConstantPoolIndexGrammar)
  for (classWithConstantReferences <- constantReferences) {
    val shape: NodeShape = classWithConstantReferences._1
    val constantReferences: Map[NodeField, NodeShape] = classWithConstantReferences._2
    val classGrammar: BiGrammar = find(shape)
    for (constantReference <- constantReferences) {
      val field = constantReference._1
      val constantType = constantReference._2
      val fieldGrammar: GrammarReference = classGrammar.findAs(field)
      val constantReferenceGrammar: GrammarReference = fieldGrammar.findGrammar(constantPoolIndexGrammar).get
      val constantElementGrammar: BiGrammar = find(constantType)
      constantReferenceGrammar.set(constantElementGrammar)
    }
  }
}
```

The import on the first line of the body puts the method `find` in scope which we use a couple of times. The code up until the last two lines can be summarized as:

1. Looks up the grammar for a class
1. Look up the grammar for a field in that class
1. Look up the grammar for a constant pool reference in that field grammar

We need to follow these steps so that we find the correct constant pool reference grammar. If we would just search from the root and stop at the first constant pool reference we find, then we wouldn't know what type of constant pool element it's referring to.

The last line is the only one that makes an actual change: it changes the constant pool reference grammar into the correct constant pool element grammar.