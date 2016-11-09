Blender [![Build Status](https://travis-ci.org/keyboardDrummer/Blender.svg?branch=master)](https://travis-ci.org/keyboardDrummer/Blender) [![Join the chat at https://gitter.im/LanguageBlender/Lobby#](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/LanguageBlender/Lobby#?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
===============

Blender is a language workbench, which is a tool to construct programming languages. A popular example of a language workbench is <a href="https://www.jetbrains.com/mps/">Jetbrain's MPS</a>. 

Blender's main goal is to help create modular and thus re-usable languages, allowing you to combine features from existing languages to create new ones. Blender gets modularity right by allowing you to both extend and constrain an existing language. Other language workbenches support only extension, which allows you to grow a small language into a bigger one, but not to transform between arbitrary ones. 

Another differentiator of Blender is its meta language. The meta language is the language used to define new languages. Unlike other tools, Blender's meta languages are embedded in a host language, allowing you to use Blender while inside a powerful general purpose programming language. Blender uses <a href="http://www.scala-lang.org/">Scala</a> as its host language. Some workbenches like <a href="https://github.com/usethesource/rascal">Rascal</a> define a stand-alone meta language, which provides a smooth experience when using its language construction features, but leaves you without the ecosystem of a popular language. Other workbenches like <a href="http://metaborg.org/en/latest/">Spoofax</a> and <a href="https://www.jetbrains.com/mps/">MPS</a> even define several meta languages, that each focus on different aspects of language definition such as syntax or typing rules. While these languages are user-friendly, they are often not programming languages, so they miss out on a lot of power.

###Delta's
The core concept of Blender is a *delta*. A delta is piece of code that applies a small change to a language, such as adding/removing a language feature, or adding an optimization. Delta's are put into an ordered list to form a language. Language re-use comes from re-using these delta's. Some delta's depend on others but there's a lot of freedom in combining them. A similar approach is described in the paper '*A Nanopass Framework for Compiler Education*'.

A delta can include one or several phases such as parsing, type checking, optimization and code generation.
Commonly compilers are modularised by defining several compiler phases and using a limited number of intermediate languages.
Blender focuses purely on intermediate languages to achieve modularisation.
By keeping the abstract syntax tree untyped, it becomes relatively simple to define a language as a sequence of language delta's.

###GUI
Blender includes a GUI. You can use this to play around with the defined particles and construct a compiler from them.
Once you're happy with your compiler you can play around with it in the compiler cockpit. Here you can run your compiler,
and do things like ask the compiler for its in- and output grammar.

###BiGrammar
To enable parsing and printing with little development effort, Blender uses a 'BiGrammar'.
A BiGrammar defines a bidirectional mapping between text and an AST.
The approach taken here is very similar to that described by the paper '*Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing*'.
A BiGrammar may be defined in a left recursive fashion because our implementation uses packrat parsing as described in
'*Packrat Parsing: Simple, Powerful, Lazy, Linear Time*' to deal with problems associated with such grammars.

###State
Currently I'm only working on particles that enable a Java to JVM bytecode translation. I'm using JVM as a target because it's a relatively high level language so it's easy to compile to. My current goal is to finish or almost finish a Java compiler. I'd say it's about halfway done. After that I'd like to use very different languages for front- and back-end, say Haskell as a front-end and Javascript as a back-end. I'm curious what the similarities between these languages from different paradigms are and how many compiler particles can be reused between different languages.

### Build instructions
1. Install <a href="http://www.scala-sbt.org/">sbt</a>
2. Call 'sbt run' in the project root

###Introduction Video
<a href="http://www.youtube.com/watch?feature=player_embedded&v=IHFHcf61g-k
" target="_blank"><img src="http://img.youtube.com/vi/IHFHcf61g-k/0.jpg" 
alt="Introduction video" width="240" height="180" border="10" /></a>
