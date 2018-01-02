---
title: Introduction
category: BiGrammar
order: 1
---

BiGrammar is Blender's metalanguage for everything syntax related. Its main strength is that it defines both a parser and a printer at the same time. Operators in BiGrammar will often have an equivalent effect on both the parser and the printer, although some are asymmetrical. In this article we'll go through code examples showcasing the most important operators. Afterwards you will comfortably read BiGrammar code.

#### Parsing & printing
Here is a grammar for a common while loop:

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
Shape: While
Condition: 
  Shape: Variable
  Name: i
Body:
  - Shape: Decrement
    Target: i
  - Shape: PlusEquals
    Target: x
    Value: 
      Shape: Constant
      Value: 2  
```

Also using the above grammar, we can then pretty print the AST to get:

```java
while(i) {
  i--;
  x += 2;
}
```

#### Choice, ignore and value
The following grammar maps "yes" and "no" strings to their corresponding boolean values:

```scala
"yes" ~> value(true) | "no" ~> value(false)
```

- `|` is the choice operator. If the left grammar fails, then the right grammar is used.
- The `<` and `>` symbols can be added to the end of existing sequence operators to create a variation of that operator, `<` means ignore the result of the grammar to the right. For example, `a ~> b` means `a` left of `b`, but ignore the result of `a` when parsing.
- `value` has no effect on the syntax, it will produce a value when parsing and consume one while printing.

#### Regex and map
The following grammar maps an integer to its string representation:

```scala
new RegexGrammar("""-?\d+""".r).map[String, Int](
  afterParsing = digits => Integer.parseInt(digits),
  beforePrinting = int => int.toString
)
```

- `"""-?\d+""".r` returns a regular expression that machines a sequence of digits, with an optional minus sign in front of it.
- `RegexGrammar` turns a regular expression into a grammar
- `map` transforms the subject grammar's value using a bidirectional mapping. The argument `afterParsing` is called after parsing. In this case it parses the string of digits into an `Int`. The argument `beforePrinting` is called before printing. In this case it converts the `Int` back into its string representation.

#### Next
Now that you're comfortable reading BiGrammar, continue to see [how BiGrammar supports modular language design](http://keyboarddrummer.github.io/Blender/grammar/modularity/).
