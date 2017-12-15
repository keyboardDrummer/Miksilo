---
title: Introduction
category: BiGrammar
order: 1
---

BiGrammar is Blender's metalanguage for everything syntax related, whose main strength is that it defines both a parser and a printer at the same time. Operators in BiGrammar will often have an equivalent effect on both the parser and the printer, although some are asymmetrical. In the rest of this article we'll go through some examples that showcase the most important operators. Afterwards you will comfortably read grammars defined using BiGrammar.

#### Basics
Here is a grammar for a while loop:

```scala
"while" ~ expression.inParenthesis.as(Condition) ~~ "{" %
        statement.manyVertical.indent(2).as(Body) %
        "}" asNode While
```

The grammars for `expression` and `statement` have been left out for brevity.

The operators have the following meaning:

- `~` is horizontal separation.
- `~~` is the same as `~` when parsing, but when printing adds a space between the left and right grammar. 
- `%` is vertical separation.
- `as` binds the grammar to a field in the AST
- `asNode` binds the grammar to a node in the AST
- `indent` indents the grammar on which it is applied, but only when printing.

With the BiGrammar defined above, we can parse the following program (the ugly formatting is intentional):

```java
while (i){
  i--; x += 2;
}
```

which yields this AST:

```yml
Class: While
Condition: 
  Class: Variable
  Name: i
Body:
  - Class: Decrement
    Target: i
  - Class: PlusEquals
    Target: x
    Value: 
      Class: Constant
      Value: 2  
```

Also using the above grammar, we can then pretty printed the AST to get:

```java
while(i) {
  i--;
  x += 2;
}
```

#### Choice, ignore and value
This grammar maps yes and no strings to their corresponding boolean values:

```scala
"yes" ~> value(true) | "no" ~> value(false)
```

- `|` is the choice operator. If the left grammar fails to parse/print, then the right grammar is used.
- The `<` and `>` symbols can be added to the end of existing sequence operators to create a variation of that operator, `<` means ignore the result of the grammar to the right. For example, `a ~> b` means `a` left of `b`, but ignore the result of `a` when parsing.
- `value` has no effect on the syntax, it will produce a value when parsing and consume one while printing.

#### Regex and map
This grammar maps integers to their string representation:

```scala
new RegexGrammar("""-?\d+""".r).map(
  afterParsing = (s: String) => Integer.parseInt(s), 
  beforePrinting =  (i: Int) => Some(i.toString)
)
```

- `RegexGrammar` turns a regular expression into a grammar
- `map` transforms the grammar's value using a bidirectional mapping. The argument `beforePrinting` returns an `Option` to allow printing to fail if the passed value is not mapped by the grammar.

#### Next
Now that you're comfortable reading BiGrammar, continue to see [how BiGrammar supports modular language design](http://keyboarddrummer.github.io/Blender/bigrammar/modularity/).
