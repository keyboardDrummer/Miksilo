---
title: Whole language transformation
category: BiGrammar
order: 3
---

In [Modularity](http://keyboarddrummer.github.io/Blender/bigrammar/modularity/) we showed some of the features of BiGrammar that enable modularity. In this article, we’ll demonstrate the extent of BiGrammar’s modularity by showing off delta’s that change the entire grammar of a language.

Our first case starts with a simple refactoring which reorders the members of a Java class, so that static fields are placed before instance fields. The problem is that this refactoring is incomplete: it only works on Java programs without comments. We'll demonstrate a series of three delta's that together enable the refactoring to accept Java block comments in the input program, and also to output them in the refactored program, in an way that matches with how we use comments.

Our input program for this case is the following:
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
We can allow comment parsing using a very simple delta, but to understand how that works first we need to see how whitespace parsing is defined in our example Java language. By default, languages in Blender define a grammar called `TriviaGrammar` that parses whitespace. Given a `Language`, we can use `import language.grammars._` to get a set of language specific parser combinators. These combinators, such as `~`, will use the language's `TriviaGrammar` as a separator when placing other grammars in sequence. Here is an example that defines part of the Java grammar used for our earlier program:
```scala
  override def transformGrammars(language: Language): Unit = {
    import language.grammars._

    //Create an empty named grammar, that we can modify in another delta.
    val classMember = create(ClassMemberGrammar) 

    val nameGrammar = "class" ~~> identifier.as(ClassName)
    val membersGrammar = classMember.manyVertical.indent(4).as(Members)
    val classGrammar = packageGrammar % importsGrammar % 
      nameGrammar ~< "{" % membersGrammar %< "}" asLabelledNode Clazz

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
If we add this delta to our refactoring, and then apply it, we get the following output program:
```scala
class Example {
    static int second;
    int first;
}
```
The reordering has completed but all the comments are gone! To retain our comments in the output, we'll add another delta that causes the results from `TriviaGrammar` to be stored in the AST. For brevity we won't show the code now, but you can find it [here](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/trivia/CaptureTriviaDelta.scala). With this delta added to our refactoring, we get the following output:
```scala
class Example {
    public static /* bar */ int second;
    /* second is used for foo */
    int first;
}
```
This is quite good! All the comments are still there in the output, and the `/* bar */` comment inside the second field has moved with the field. However, one last thing still irks us. The comment `/* second is used for foo */` is in the same position as before, in front of the field `first`, even though it is meant to clarify the meaning of the field `second`.

The reason for this is that the comment, because it is located between two fields, is stored not in one of the field nodes, but in the class node. We can add one last delta that will improve this behavior, so that trivia located right in front of a node, will be stored inside that node, instead of the parent node. The code is shown [here](https://github.com/keyboardDrummer/Blender/blob/master/src/main/scala/deltas/javac/trivia/TriviaInsideNode.scala). The resulting output is now:

```scala
class Example {
    /* second is used for foo */
    public static /* bar */ int second;
    int first;
}
```

Perfect!