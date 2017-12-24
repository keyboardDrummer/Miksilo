package core.bigrammar.grammars

import core.bigrammar.printer.UndefinedDestructuringValue

class IgnoreLeft(inner: Sequence) extends MapGrammar(inner,
  { case (l, r) => r },
  r => Some((UndefinedDestructuringValue, r))) {

  def sequence: Sequence = inner.asInstanceOf[Sequence]
}
