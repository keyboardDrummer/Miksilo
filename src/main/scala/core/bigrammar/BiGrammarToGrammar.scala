package core.bigrammar

import core.grammar.Grammar

object BiGrammarToGrammar {
  object Observer extends BiGrammarObserver[Grammar] {
    override def labelledEnter(name: AnyRef): Grammar = new core.grammar.Labelled(name)

    override def handleGrammar(self: BiGrammar, recursive: (BiGrammar) => Grammar): Grammar = self match {
      case sequence: SequenceLike => core.grammar.Sequence(recursive(sequence.first), recursive(sequence.second))
      case choice:Choice => core.grammar.Choice(recursive(choice.left), recursive(choice.right), choice.firstBeforeSecond)
      case Consume(consume) => consume
      case Keyword(keyword, reserved) => core.grammar.Keyword(keyword, reserved)
      case Delimiter(keyword) => core.grammar.Delimiter(keyword)
      case many:Many => core.grammar.Many(recursive(many.inner))
      case mapGrammar: MapGrammar => core.grammar.MapGrammar(recursive(mapGrammar.inner), mapGrammar.construct)
      case BiFailure => core.grammar.FailureG()
      case Print(_) => core.grammar.Produce(Unit)
      case Produce(value) => core.grammar.Produce(value)
    }

    override def labelledLeave(inner: Grammar, partial: Grammar): Unit = partial.asInstanceOf[core.grammar.Labelled].inner = inner
  }
  
  def toGrammar(grammar: BiGrammar): Grammar = Observer.observe(grammar)
}
