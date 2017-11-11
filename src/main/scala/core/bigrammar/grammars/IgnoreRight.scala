package core.bigrammar.grammars

import core.grammar.~

class IgnoreRight(inner: SequenceLike) extends MapGrammar(inner,
  { case ~(l, r) => l},
  l => Some(core.grammar.~(l, UndefinedDestructuringValue)))
