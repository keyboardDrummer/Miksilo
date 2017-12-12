---
title: Modularity
category: BiGrammar
order: 2
---
The #1 focus of Blender is to enable _modular_ language design. This page explains how BiGrammar supports that goal.

In [Unified parsing and printing](http://keyboarddrummer.github.io/Blender/bigrammar/unified-parsing-and-printing/) we introduced the `as` operator which binds a grammar to a field in the AST. This method of mapping a grammar to an AST, where the binding to a field is separate from the binding to a node, is a bit peculiar. It is common when using parser combinators, to first parse tuples of values using the sequence combinator, `~` in our case, and then apply a function to map those tuples to an AST node, for example:

```scala
(expression ~< "|" ~ expression).map(
      { case ~(left, right) => new Or(left, right) }, //Constructor
      (or: Or) => Some(~(or.left, or.right))) //Destructor
```

Now suppose we want a grammar that has both `|` and `||`, to indicate whether to evaluate the arguments lazily or strict. If we would write this grammar from scratch, we could write:

```scala
val strict = ("|" ~> value(false) | value(true))
val inner = expression ~< "|" ~ strict ~ expression
inner.map({ case ~(~(left, strict: Boolean), right) => Or(left, right, strict) },
          (or: Or) => Some(new ~(new ~(or.left, or.strict), or.right)))
```

However, if we want to get this second grammar by transforming the first, then things will get messy. We'll need to change code in multiple locations, both in the `inner` grammar, but also in both functions passed to `map`.

Now let's use the `as` and `asNode` style to bind our grammar to the AST. The initial grammar is

```scala
val grammar = expression.as(Left) ~ "|" ~ expression.as(Right) asNode Or
```
which we can transform using

```scala
val strict = ("|" ~> value(false) | value(true)).as(Strict)
grammar.findAs(Right).replace(original => strict ~ original)
```
to get the grammar with lazyness

```scala
val lazyGrammar = expression.as(Left) ~< "|" ~ strict ~ expression.as(Right) asNode Or
```
We can transform back to the initial grammar using

```scala
grammars.findAs(Strict).removeFromSequence()
```

Here are the explanations for some of the methods used:
- `findAs` traverses a `BiGrammar` and returns a `GrammarReference`, which is a [zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure)) for grammars. It defines a path through the grammar, starting at a root grammar. It is a convenient type for traversing and updating a grammar.
- `replace` changes the reference to this grammar to a new grammar passed to `replace`.
- `removeFromSequence` assumes that the grammar is to the left or right of a sequence operator such as `~`, and replaces that sequence operator with the operator that is not the current grammar. 

For example: `(a ~ b).findAs(a).removeFromSequence()` results in `b`