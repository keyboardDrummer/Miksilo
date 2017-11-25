---
title: ' '
layout: default
---

{% include home-banner.html %}

Blender is a language workbench, which is a tool to construct programming languages. A popular example of a language workbench is <a href="https://www.jetbrains.com/mps/">Jetbrain's Meta Programming System</a>. Language workbenches often define one or several _metalanguages_, which are used to define (parts of) a language.

Blender's focus is on modular language design, it gets modularity right because of these properties:

- Languages are composed of many small transformations
- Language transformations are packaged as re-usable units with well-defined dependencies
- Metalanguages are embedded, making them first class objects and allowing arbitrary computations over them. This enables transforming between arbitrary languages.
- Metalanguages are designed to be transformed. For example, early binding to fields in the grammar allows easy & safe editing.
- Compiler phases are defined generically, making them resilient to changes in the language
- Abstract syntax trees are stored unstructured, which provides many benefits such as fast transformations and allowing language composition at run-time

Blender is written in [Scala](http://www.scala-lang.org/"), which serves as a host language for its embedded metalanguages. Some workbenches like [Rascal](https://github.com/usethesource/rascal") define stand-alone metalanguages with general purpose programming facilities. This provides a smooth experience when using the language, but leaves you without the ecosystem of a popular language. Other workbenches like [Spoofax](http://metaborg.org/en/latest/) and [MPS](https://www.jetbrains.com/mps/) define metalanguages optimized for usability, leaving out generic programming facilities. While these languages are user-friendly, they are not suitable for being transformed.

> TODO. Add awesome cases.