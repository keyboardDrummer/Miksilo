---
title: ' '
layout: default
---

{% include home-banner.html %}

Blender is a [language workbench](https://en.wikipedia.org/wiki/Language_workbench), which is a tool to construct programming languages. A popular example of a language workbench is <a href="https://www.jetbrains.com/mps/">Jetbrain's Meta Programming System</a>. Language workbenches often define one or several _metalanguages_, which are used to define (parts of) a language.

Blender's focus is on modular language design. These are the properties that make it so modular:

- Languages are composed of many small transformations
- Language transformations are packaged as reusable units with well-defined dependencies
- Metalanguages are embedded in [Scala](http://www.scala-lang.org/"), making them first class objects and allowing arbitrary computations over them. This enables transforming between arbitrary languages.
- Metalanguages are designed to be transformed. For example, early binding to fields in the grammar allows easy & safe editing.
- Compiler phases are defined generically, making them resilient to changes in the language
- Abstract syntax trees are stored unstructured, which provides many benefits such as fast transformations and language composition at run-time

The best way to show Blender's modularity is by example. Here are examples of language transformations demonstrating the power of Blender:

- [Add support for comments in a language agnostic way.](http://keyboarddrummer.github.io/Blender/bigrammar/modularity/)
- [Inline the constant pool in Java bytecode.](http://keyboarddrummer.github.io/Blender/deltas/inline-constant-pool/)
- [Resolving interactions between independent delta's.](http://keyboarddrummer.github.io/Blender/deltas/delta-interactions/)

Before diving into the above, it might help to get [an introduction of BiGrammar](http://keyboarddrummer.github.io/Blender/bigrammar/introduction/), one of Blender's metalanguages.