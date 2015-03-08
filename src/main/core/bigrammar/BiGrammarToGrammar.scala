package core.biGrammar

import core.grammar.Grammar

object BiGrammarToGrammar {
  object Observer extends BiGrammarObserver[Grammar] {
    override def labelledEnter(name: AnyRef): Grammar = new core.grammar.Labelled(name)

    override def handleGrammar(self: BiGrammar, recursive: (BiGrammar) => Grammar): Grammar = self match {
      case sequence: SequenceLike => core.grammar.Sequence(recursive(sequence.first), recursive(sequence.second))
      case Choice(first, second) => core.grammar.Choice(recursive(first), recursive(second))
      case Consume(consume) => consume
      case Keyword(keyword) => core.grammar.Keyword(keyword)
      case Delimiter(keyword) => core.grammar.Delimiter(keyword)
      case Many(inner) => core.grammar.Many(recursive(inner))
      case MapGrammar(inner, construct, _) => core.grammar.MapGrammar(recursive(inner), construct)
      case BiFailure => core.grammar.FailureG
      case Print(_) => core.grammar.Produce(Unit)
      case Produce(value) => core.grammar.Produce(value)
    }

    override def labelledLeave(inner: Grammar, partial: Grammar): Unit = partial.asInstanceOf[core.grammar.Labelled].inner = inner
  }
  
  def toGrammar(grammar: BiGrammar): Grammar = Observer.observe(grammar)
}
