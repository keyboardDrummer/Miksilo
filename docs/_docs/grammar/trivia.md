---
title: Trivia
category: BiGrammar
order: 3
---

> Refer to the delta article as a dependency

In [this previous article on modularity](http://keyboarddrummer.github.io/Blender/grammar/modularity/), we showed some of BiGrammar's features that enable modularity. Here, we’ll demonstrate the extent of BiGrammar’s modularity by showing off delta’s that change the entire grammar of a language.

To demonstrate these deltas, we need a program transformation, such as a refactoring or a compilation, for them to transform. We choose a simple refactoring called _reorder members_, that reorders the members of a Java class, so that static fields are placed before instance fields. The problem is that this refactoring is incomplete: it only works on Java programs without comments. A series of three deltas will enable the refactoring to accept Java block comments in the input program, and to output them in the refactored program, in an way that matches with how programmers use comments.

Our input program for this case is:

```scala
class Example {
    int first;
    /* second is used for foo */
    public static /* bar */ int second;
}
```

If we apply reorder members on this program, we get the following exception:

```java
[5.5] failure: `}' expected but `/' found

    /* second is for XYZ */
    ^
```

We can enable the parsing of comment using a very simple delta, but to understand how that works first we need to see how whitespace parsing is defined in our example Java language. By default, languages in Blender define a grammar called `TriviaGrammar` that parses whitespace. Given a `Language`, we can use `import language.grammars._` to get a set of language specific parser combinators. These combinators, such as `~`, will use the language's `TriviaGrammar` as a separator when placing other grammars in sequence. Here is an example that defines part of the Java grammar used for our earlier program:

```scala
  override def transformGrammars(language: Language): Unit = {
    import language.grammars._

    //Create an empty named grammar, that we can modify in another delta.
    val classMember = create(ClassMemberGrammar) 

    val nameGrammar = "class" ~~> identifier.as(ClassName)
    val membersGrammar = classMember.manyVertical.indent(4).as(Members)
    val classGrammar = nameGrammar ~< "{" % membersGrammar %< "}" asLabelledNode Shape

    //BodyGrammar is the file's top level grammar
    find(BodyGrammar).inner = classGrammar 
  }
```

Now that we know about TriviaGrammar, we can understand how the following Delta adds comment parsing to the language:

```scala
object JavaStyleCommentsDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, state: Language) = {
    val commentGrammar = RegexGrammar("""(?s)/\*.*?\*/""".r)
    grammars.find(TriviaGrammar).addOption(getCommentGrammar)
  }
}
```

If we add this delta to reorder members, and then run it on the input from before, we get the following output:

```scala
class Example {
    static int second;
    int first;
}
```

The reordering has completed but all of the comments are gone! To retain our comments in the output, we'll add another delta, [StoreTriviaDelta](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/trivia/StoreTriviaDelta.scala), that causes the results from `TriviaGrammar` to be stored in the AST. With this delta added to reorder members, we get the following output:

```scala
class Example {
    public static /* bar */ int second;
    /* second is used for foo */
    int first;
}
```

Now we're getting somewhere. All the comments are still present in the output, and the `/* bar */` comment inside the second field has moved with the field. However, one last thing still bothers us. The comment `/* second is used for foo */` is in the same position as before, in front of the field `first`, even though it is meant to clarify the meaning of the field `second`.

The reason for this is that the comment, because it is located between two fields, is stored not in one of the field nodes, but in the class node. We can add one last delta, [TriviaInsideNode](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/trivia/TriviaInsideNode.scala), that will improve this behavior, so that trivia located right in front of a node will be stored inside that node, instead of the parent node. The resulting output is then:

```scala
class Example {
    /* second is used for foo */
    public static /* bar */ int second;
    int first;
}
```

Perfect!

The refactoring used as a demonstration in this article is a minimal example, but since the three deltas we used are all language agnostic, they can be used in any context, for example when transforming from `Java` to `C#`.