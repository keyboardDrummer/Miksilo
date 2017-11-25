---
title: Blender
---

Blender is a language workbench, which is a tool to construct programming languages. A popular example of a language workbench is <a href="https://www.jetbrains.com/mps/">Jetbrain's Meta Programming System</a>.

Blender's focus is to enable creating modular and thus re-usable languages, allowing you to combine features from existing languages to create new ones. Blender gets modularity right by allowing you to both extend and constrain an existing language. Other language workbenches support only extension, which allows you to grow a small language into a bigger one, but not to transform between arbitrary languages.

Another differentiator of Blender is its meta languages. A meta language is a language used to define (parts of) a language. Unlike with other tools, Blender's meta languages are embedded in a host language, [Scala](http://www.scala-lang.org/"), allowing you to use them while inside a powerful general purpose programming language. Some workbenches like [Rascal](https://github.com/usethesource/rascal") define a stand-alone meta language, which provides a smooth experience when using its language construction features, but leaves you without the ecosystem of a popular language. Other workbenches like [Spoofax](http://metaborg.org/en/latest/) and [MPS](https://www.jetbrains.com/mps/) define several meta languages, that each focus on different aspects of language definition such as syntax or typing rules. While these languages are user-friendly, they are often not programming languages, so they miss out on a lot of power.

### Delta
The core concept of Blender is a *delta*. A delta applies a small change to a language, such as adding/removing a language feature, or adding an optimization. Delta's can be chained together to form a language. Language re-use comes from re-using these delta's. Some delta's depend on others but there's a lot of freedom in combining them. A similar approach is described in the paper [A Nanopass Framework for Compiler Education](https://www.cs.indiana.edu/~dyb/pubs/nano-jfp.pdf).

### BiGrammar
To allow writing both a parser and a printer at the same time, Blender defines the [BiGrammar DSL](https://github.com/keyboardDrummer/Blender/wiki/BiGrammar-1:-unified-parsing-and-printing). The approach taken here is similar to that described by the paper [Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing](http://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf).
A BiGrammar may be defined in a left recursive fashion, which can be contributed to the use of packrat parsing as described in
[Packrat Parsing: Simple, Powerful, Lazy, Linear Time](http://bford.info/pub/lang/packrat-icfp02.pdf) to deal with problems associated with such grammars.

### GUI
Blender includes a GUI. You can use this to play around with the defined deltas and construct a language from them.
Once you're happy with your language you can play around with it in the language cockpit. Here you can run write programs in the language with IDE support, compile those programs and pretty print the target language, run those programs, determine which compilation phases the language's compiler has, and inspect the language's in- and output grammar.

To see Blender's GUI in action, watch our [introduction video](http://www.youtube.com/watch?feature=player_embedded&v=IHFHcf61g-k).

### Build instructions
1. Install <a href="http://www.scala-sbt.org/">sbt</a>
2. Call 'sbt run' in the project root

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
1. If you decide to contribute, it might help to reach out to rgv.willems@gmail.com, so he can help you out where the documentation is lacking.
1. Look through the open issues to see if you find something interesting
1. Start hacking ;-)
1. Once you're done, submit a pull request and make sure the build server approves it.
