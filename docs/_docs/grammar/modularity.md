---
title: Modularity
category: BiGrammar
order: 2
---
The goal of Blender is to enable _modular_ language design. This article shows how BiGrammar supports that.

In [the introduction to BiGrammar](http://keyboarddrummer.github.io/Blender/grammar/introduction/), we introduced the `as` operator which binds a grammar to a field in the AST. This method of mapping a grammar to an AST, where the binding to a field is separate from the binding to a node, is peculiar. More commonly, users of parser combinators first parse tuples of values using the sequence combinator, `~` in our case, and then apply a function to map those tuples to an AST node. We can demonstrate this approach using BiGrammar; as an example we define the strict boolean or operator `|`:

```scala
case class Or(left: Expression, right: Expression) extends Expression

val orGrammar = (expression ~< "|" ~ expression).map(
      { case (left, right) => new Or(left, right) }, //constructor
      (or: Or) => Some(or.left, or.right)) //destructor
```

In the constructor, the tuple created by the `~` operator is pattern matched on (Scala uses `,` to construct a tuple), and transformed into a custom type `Or`. In the destructor, the `Or` is pattern matched on and transformed into a tuple wrapped in a `Some`.

Now suppose we want a grammar that has both the strict `|` and the lazy `||` operators. We could implement this by first parsing one pipe, and then optionally parsing a second pipe to determine if the operator is lazy. If we would write this grammar from scratch, we could simply make some additions to our previous definition:

```scala
case class Or(left: Expression, right: Expression, lazily: Boolean) extends Expression

val lazyPipe = "|" ~> value(true) | value(false)
val inner = expression ~< "|" ~ lazyPipe ~ expression
val orGrammar = inner.map(
          { case (left, lazily: Boolean, right) => Or(left, right, lazily) },
          (or: Or) => Some(or.left, or.lazily, or.right))
```

Clearly the two definitions are similar, so from a [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) perspective it makes sense to define one in terms of the other. However, if we want to get the grammar with laziness by transforming the one without it, then things will get messy. We'll need to change code in multiple locations. In the `inner` grammar we need to add the reference to `lazyPipe`, and we need to modify both functions passed to `map`.

Now let's switch to the idiomatic way of writing BiGrammar grammars. We'll use the `as` and `asNode` operators to bind our grammar to the AST. The initial grammar is:

```scala
object Or extends NodeShape
object Left extends NodeField
object Right extends NodeField

val orGrammar = expression.as(Left) ~ "|" ~ expression.as(Right) asNode Or
```

An immediate advantage is that we don't have to unwrap and wrap the tuples any more, saving us some boilerplate. However, a more important advantage is that we can easily transform this grammar. Here we add the `||` operator:

```scala
object Lazy extends NodeField

val lazily = ("|" ~> value(true) | value(false)).as(Lazy)
grammar.findAs(Right).replace(original => lazily ~ original)
```

Here are explanations for the methods used:

- `findAs` traverses a `BiGrammar` to find a particular `as` binding, and returns the path it took through the grammar. The return type is a `GrammarPath`, which is a [zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure)) for `BiGrammar`.
- `replace` takes the latest reference traversed by the grammar path and sets it to the grammar that is passed to `replace`.

Now if we had started with the grammar with laziness, and we would have wanted to get the one without, we could have written:

```scala
orGrammar.findAs(Lazy).removeFromSequence()
```

`removeFromSequence` assumes that the grammar is the `first` or `second` field of a sequence operator such as `~`, and replaces that sequence operator with the field (`first` or `second`) that is not the current grammar. Here is an example: `(a ~ b).find(a).removeFromSequence()` results in `b`

This article has demonstrated how BiGrammar supports modularity. Early binding of grammars to fields makes mutations easier to write, and the zipper `GrammarPath` provides utilities for easily editing grammars. Note that this depends on us sidestepping the type system, otherwise binding to fields without calling the constructor would not be possible. If you'd like to see the extent of BiGrammar's modularity, check out [this transformation](http://keyboarddrummer.github.io/Blender/grammar/trivia/) which adds comments to a language in a language agnostic way.