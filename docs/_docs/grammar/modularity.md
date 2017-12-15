---
title: Modularity
category: BiGrammar
order: 2
---
The goal of Blender is to enable _modular_ language design. This article shows how BiGrammar supports that.

In [the introduction to BiGrammar](http://keyboarddrummer.github.io/Blender/bigrammar/introduction/), we introduced the `as` operator which binds a grammar to a field in the AST. This method of mapping a grammar to an AST, where the binding to a field is separate from the binding to a node, is peculiar. More commonly, users of parser combinators first parse tuples of values using the sequence combinator, `~` in our case, and then apply a function to map those tuples to an AST node. We can demonstrate this approach using BiGrammar:

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

However, if we want to get the grammar with strict by transforming the grammar without it, then things will get messy. We'll need to change code in multiple locations, both in the `inner` grammar, but also in both functions passed to `map`.

Now let's use the `as` and `asNode` style to bind our grammar to the AST. The initial grammar is:

```scala
val grammar = expression.as(Left) ~ "|" ~ expression.as(Right) asNode Or
```

The immediate advantage is that we don't have to unwrap and wrap the tuples values any more, saving us some boilerplate. However, a more important advantage is that we can easily transform this grammar to include the strict operator:

```scala
val strict = ("|" ~> value(false) | value(true)).as(Strict)
grammar.findAs(Right).replace(original => strict ~ original)
```

We can transform back to the initial grammar using:

```scala
grammars.findAs(Strict).removeFromSequence()
```

Here are explanations for the methods used:

- `findAs` traverses a `BiGrammar` to find a particular `as` binding, and returns the path it took through the grammar. The return type is a `GrammarPath`, which is a [zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure)) for `BiGrammar`.
- `replace` takes the latest reference traversed by the grammar path and sets it to the grammar that is passed to `replace`.
- `removeFromSequence` assumes that the grammar is the `first` or `second` field of a sequence operator such as `~`, and replaces that sequence operator with the field (`first` or `second`) that is not the current grammar. Here is an example: `(a ~ b).findAs(a).removeFromSequence()` results in `b`