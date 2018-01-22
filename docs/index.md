---
title: ' '
layout: default
---

{% include home-banner.html %}

Miksilo is a [language workbench](https://en.wikipedia.org/wiki/Language_workbench), which is a tool to construct programming languages. Miksilo, whose name comes from the [Esperanto](https://en.wikipedia.org/wiki/Esperanto) word for mixer, lets you create languages quickly by mixing existing languages and building on top of them.

Language construction is notoriously hard, and these days involves not just writing a compiler or interpreter, but also tooling to include in an [IDE](https://en.wikipedia.org/wiki/Integrated_development_environment). Language workbenches make language design accessible to a larger audience; they allow writing more [domain-specific languages](https://en.wikipedia.org/wiki/Domain-specific_language), and prototyping new languages.

Because languages share many properties, the quickest way to build a language is to build on top of existing ones. Although most language workbenches allow some language reuse, Miksilo is built for modular language design. To achieve that goal, it has the following properties:

- Languages are composed of many small transformations.
- Language transformations are packaged as reusable units with explicit dependencies between them.
- An important part of language workbenches is that they often define one or several _metalanguages_, which are used to define (parts of) a language. Miksilo's metalanguages are embedded in [Scala](http://www.scala-lang.org), which allows arbitrary computations over them, and enables transforming between arbitrary languages.
- Metalanguages are designed to be transformed. For example, early binding to fields in the grammar allows easy & safe editing.
- Compiler phases are defined generically, making them resilient to changes in the language.
- Abstract syntax trees are stored unstructured, which provides benefits such as fast transformations and language composition at run-time.

The best way to show Miksilo's modularity is by example, so we've picked a few language transformations as showcases. Before diving into them however, we recommend to get [an introduction to BiGrammar](http://keyboarddrummer.github.io/Miksilo/grammar/introduction/), one of Miksilo's metalanguages. Here are the showcases:

1. [Add support for comments in a language agnostic way.](http://keyboarddrummer.github.io/Miksilo/grammar/trivia/)
1. [Inline the constant pool in Java bytecode.](http://keyboarddrummer.github.io/Miksilo/deltas/inline-constant-pool/)
1. [Resolving interactions between independent deltas.](http://keyboarddrummer.github.io/Miksilo/deltas/delta-interactions/)

If you would prefer to learn Miksilo by experimenting instead of reading, then try out its [sandbox](http://keyboarddrummer.github.io/Miksilo/core/sandbox/) application, which lets you create languages by composing predefined language transformations.