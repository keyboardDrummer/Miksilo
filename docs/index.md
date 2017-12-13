---
title: ' '
layout: default
---

{% include home-banner.html %}

Blender is a [language workbench](https://en.wikipedia.org/wiki/Language_workbench), which is a tool to construct programming languages. These tools often define one or several _metalanguages_, which are used to define (parts of) a language.

Language construction is notoriously hard, and these days involves not just writing a compiler or interpreter, but also tooling to include in an [IDE](https://en.wikipedia.org/wiki/Integrated_development_environment). Language workbenches make language design accessible to a larger audience; they allow writing more [domain-specific languages](https://en.wikipedia.org/wiki/Domain-specific_language), and prototyping new types of languages.

Because languages share many properties, the quickest way to build a language is to build on top of existing ones. Although most languages workbenches allow some language reuse, Blender is built for modular language design. To achieve that goal, it has these properties:

- Languages are composed of many small transformations.
- Language transformations are packaged as reusable units with explicit dependencies.
- Metalanguages are embedded in [Scala](http://www.scala-lang.org), making them first class objects and allowing arbitrary computations over them. This enables transforming between arbitrary languages.
- Metalanguages are designed to be transformed. For example, early binding to fields in the grammar allows easy & safe editing.
- Compiler phases are defined generically, making them resilient to changes in the language.
- Abstract syntax trees are stored unstructured, which provides benefits such as fast transformations and language composition at run-time.

The best way to show Blender's modularity is by example. Here are examples of language transformations demonstrating the power of Blender:

- [Add support for comments in a language agnostic way.](http://keyboarddrummer.github.io/Blender/bigrammar/modularity/)
- [Inline the constant pool in Java bytecode.](http://keyboarddrummer.github.io/Blender/deltas/inline-constant-pool/)
- [Resolving interactions between independent delta's.](http://keyboarddrummer.github.io/Blender/deltas/delta-interactions/)

Before diving into the above, it might help to get [an introduction to BiGrammar](http://keyboarddrummer.github.io/Blender/bigrammar/introduction/), one of Blender's metalanguages.

Another way to get to know Blender is to play around with its [sandbox](http://keyboarddrummer.github.io/Blender/core/sandbox/) application, which lets you create languages by composing predefined language transformations.