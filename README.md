Particle Compiler
===============

Particle compiler is a tool for quickly building toy compilers. It can be used for prototyping language features or as a way of teaching compiler construction.
A compiler *particle* is piece of code that applies a small change to a compiler, such as adding/removing a language feature, or adding an optimization. Particles are put into an ordered list to form a compiler. Some particles depend on others but there's a lot of freedom in combining them. A similar approach is described in the paper '*A Nanopass Framework for Compiler Education*'.

###Particles
A particle can include one or several phases such as parsing, type checking, optimization and code generation.
Commonly compilers are modularised by defining several compiler phases and using a limited number of intermediate languages.
Particle compiler focuses purely on intermediate languages to achieve modularisation.
By keeping the abstract syntax tree untyped, it becomes relatively simple to define a language as a sequence of language delta's.

###GUI
Particle compiler includes a GUI. You can use this to play around with the defined particles and construct a compiler from them.
Once you're happy with your compiler you can play around with it in the compiler cockpit. Here you can of course run the compiler,
but also do things like ask the compiler for its in- and output grammar.

###BiGrammar
To enable parsing and printing with little development effort, particle compiler uses a 'BiGrammar'.
A BiGrammar defines a bidirectional mapping between text and an AST.
The approach taken here is very similar to that described by the paper '*Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing*'.
A BiGrammar may be defined in a left recursive fashion because our implementation uses packrat parsing as described in
'*Packrat Parsing: Simple, Powerful, Lazy, Linear Time*' to deal with problems associated with such grammars.

###Introduction Video
<a href="http://www.youtube.com/watch?feature=player_embedded&v=IHFHcf61g-k
" target="_blank"><img src="http://img.youtube.com/vi/IHFHcf61g-k/0.jpg" 
alt="Introduction video" width="240" height="180" border="10" /></a>
