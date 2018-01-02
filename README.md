Blender [![Build Status](https://travis-ci.org/keyboardDrummer/Blender.svg?branch=master)](https://travis-ci.org/keyboardDrummer/Blender) [![Join the chat at https://gitter.im/LanguageBlender/Lobby#](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/LanguageBlender/Lobby#?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
===============

Blender is a [language workbench](https://en.wikipedia.org/wiki/Language_workbench), which is a tool to construct programming languages. An important part of these tools is that they often define one or several _metalanguages_, which are used to define (parts of) a language.

Language construction is notoriously hard, and these days involves not just writing a compiler or interpreter, but also tooling to include in an [IDE](https://en.wikipedia.org/wiki/Integrated_development_environment). Language workbenches make language design accessible to a larger audience; they allow writing more [domain-specific languages](https://en.wikipedia.org/wiki/Domain-specific_language), and prototyping new languages.

Because languages share many properties, the quickest way to build a language is to build on top of existing ones. Although most language workbenches allow some language reuse, Blender is built for modular language design. To achieve that goal, it has the following properties:

- Languages are composed of many small transformations.
- Language transformations are packaged as reusable units with explicit dependencies between them.
- Metalanguages are embedded in [Scala](http://www.scala-lang.org), which allows arbitrary computations over them, and enables transforming between arbitrary languages.
- Metalanguages are designed to be transformed. For example, early binding to fields in the grammar allows easy & safe editing.
- Compiler phases are defined generically, making them resilient to changes in the language.
- Abstract syntax trees are stored unstructured, which provides benefits such as fast transformations and language composition at run-time.

The best way to show Blender's modularity is by example, so we've picked a few language transformations as showcases. Before diving into them however, we recommend to get [an introduction to BiGrammar](http://keyboarddrummer.github.io/Blender/grammar/introduction/), one of Blender's metalanguages. Here are the showcases:

1. [Add support for comments in a language agnostic way.](http://keyboarddrummer.github.io/Blender/grammar/trivia/)
1. [Inline the constant pool in Java bytecode.](http://keyboarddrummer.github.io/Blender/deltas/inline-constant-pool/)
1. [Resolving interactions between independent deltas.](http://keyboarddrummer.github.io/Blender/deltas/delta-interactions/)

If you would prefer to learn Blender by experimenting instead of reading, then try out its [sandbox](http://keyboarddrummer.github.io/Blender/core/sandbox/) application, which lets you create languages by composing predefined language transformations.

### Build instructions
Building Blender is as simple as:

1. Grab [the source](https://github.com/keyboardDrummer/Blender) from GitHub.
1. Make sure you have installed the Java 8 JDK, or a higher version. To check what version you have installed, open the terminal and type:

   `javac -version`

   If you don't have it installed, download the JDK <a href="http://www.oracle.com/technetwork/java/javase/downloads/index.html">here</a>.
1. Install <a href="http://www.scala-sbt.org/">sbt</a>
1. Call 'sbt run' in the project root to build Blender and start the sandbox desktop application.

### Contributions
There's an infinite amount of work to be done for Blender, so contributions are very welcome. There are many different topics to work on, some suitable for a Bachelor's or Master's thesis.

Some examples of cool features:
- Parser combinators that allow defining indentation sensitive grammars, such as those of Python and Haskell.
- A DSL for static semantics, such as name binding and type checking. See the paper [A constraint language for static semantic analysis based on scope graphs](http://delivery.acm.org/10.1145/2850000/2847543/p49-antwerpen.pdf?ip=145.129.111.38&id=2847543&acc=OA&key=4D4702B0C3E38B35%2E4D4702B0C3E38B35%2E4D4702B0C3E38B35%2E77FCF3B2F09622E1&CFID=992904318&CFTOKEN=51306518&__acm__=1507451717_5c1e5970ab3ac31fbd9849edb486a802) for inspiration
- Error correcting parsing
- Generating syntactic code completion from a grammar, as in the paper [Principled syntactic code completion using placeholders](http://delivery.acm.org/10.1145/3000000/2997374/p163-amorim.pdf?ip=145.129.111.38&id=2997374&acc=OA&key=4D4702B0C3E38B35%2E4D4702B0C3E38B35%2E4D4702B0C3E38B35%2E77FCF3B2F09622E1&CFID=992904318&CFTOKEN=51306518&__acm__=1507451951_eb454d2173854f174d05e3c1e1526bbd)
- Incremental compilation: incremental parsing, incremental type checking, etc.
- Add a new language front-end or back-end.

#### How to contribute
1. Reach out on [the Gitter](https://gitter.im/LanguageBlender/Lobby), so other contributors can help you out where the documentation is lacking.
1. Look through the open issues to see if you find something interesting
1. Enjoy the work ;-)
1. Once you're done, submit a pull request and make sure the build server approves it.
