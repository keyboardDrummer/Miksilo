---
title: Unified Parsing & Printing
category: BiGrammar
order: 1
---

BiGrammar is a DSL that allows the user to define both a parser and a pretty printer at the same time. Operators and methods in BiGrammar will often have an effect on both the parser and the printer, both can sometimes influence only one of both.

### Example
Here follows an example of a small piece of grammar defined using BiGrammar:

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

With the defined BiGrammar, we can parse the following program (ugly formatting is intentional)
```java
while (i){
  i--; x += 2;
}
```

yielding this AST

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
which can be pretty printed to
```Java
while(i) {
  i--;
  x += 2;
}
```

### Choice, ignore and value
Here is another example grammar:
```scala
"yes" ~> value(true) | "no" ~> value(false)
```
- `|` is the choice operator. If the left grammar fails to parse/print, then the right grammar is used.
- The `<` and `>` symbols can be added to the end of existing operators to create a variation, `<` means ignore the result of the grammar to the right, so `a ~> b` means `a` left of `b`, but ignore the result of `a` when parsing.
- `value` has no effect on the syntax, it will produce a value when parsing and consume one while printing.

### Regex and map
Here is another example:
```scala
new RegexGrammar("""-?\d+""".r).map(
  afterParsing = (s: String) => Integer.parseInt(s), 
  beforePrinting =  (i: Int) => Some(i.toString)
)
```
- `RegexGrammar` turns a regular expression into a grammar
- `map` transforms the grammar's value using a bidirectional mapping. The argument `beforePrinting` returns an `Option` to allow printing to fail if the passed value does not belong to the grammar.

### Next 
[Read how BiGrammar supports modular language design](http://keyboarddrummer.github.io/Blender/bigrammar/modularity/)