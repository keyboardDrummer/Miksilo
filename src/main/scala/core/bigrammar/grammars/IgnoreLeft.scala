package core.bigrammar.grammars

import core.bigrammar.printer.UndefinedDestructuringValue
import core.grammar.~

class IgnoreLeft(inner: Sequence) extends MapGrammar(inner,
  { case ~(l, r) => r },
  r => Some(core.grammar.~(UndefinedDestructuringValue, r))) {
  def sequence = inner.asInstanceOf[Sequence]
}
