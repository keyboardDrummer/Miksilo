package core.bigrammar.grammars

import core.bigrammar.printer.UndefinedDestructuringValue
import core.grammar.~

class IgnoreRight(inner: Sequence) extends MapGrammar(inner,
  { case ~(l, r) => l},
  l => Some(core.grammar.~(l, UndefinedDestructuringValue)))
