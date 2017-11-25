---
title: BiGrammar
category: Grammar
order: 1
---

Blender's metalanguage for everything syntax related is called BiGrammar, which defines both a parser and a printer at the same time. The techniques underlying this are similar to those described in the paper [Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing](http://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf).

A BiGrammar may be defined in a left recursive fashion, which can be contributed to the use of packrat parsing with support for left-recursion. The work underlying this is described in the papers [Packrat Parsing: Simple, Powerful, Lazy, Linear Time](http://bford.info/pub/lang/packrat-icfp02.pdf) and [Packrat Parsers Can Support Left Recursion](http://www.vpri.org/pdf/tr2007002_packrat.pdf).

### Next
[Read how BiGrammar implements unified parsing and printing](http://keyboarddrummer.github.io/Blender/grammar/unified-parsing-and-printing/)

