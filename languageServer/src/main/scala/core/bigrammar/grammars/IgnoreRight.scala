package core.bigrammar.grammars

import core.bigrammar.printer.UndefinedDestructuringValue

class IgnoreRight(inner: Sequence) extends MapGrammar(inner,
  { case (l, r) => l},
  l => Some((l, UndefinedDestructuringValue)))
