package core.bigrammar

import core.grammar.{Grammar, ~}

case class WithMap(value: Any, state: Map[Any,Any])
{
}

object BiGrammarToGrammar {
  def addState(value: Any) = WithMap(value, Map.empty)
  object Observer extends BiGrammarObserver[Grammar] {
    override def labelledEnter(name: AnyRef): Grammar = new core.grammar.Labelled(name)

    override def handleGrammar(self: BiGrammar, recursive: (BiGrammar) => Grammar): Grammar = self match {
      case sequence: SequenceLike => core.grammar.Sequence(recursive(sequence.first), recursive(sequence.second)) ^^
        { case ~(WithMap(l,sl),WithMap(r,sr)) => WithMap(core.grammar.~(l,r), sl ++ sr)}
      case choice:Choice => core.grammar.Choice(recursive(choice.left), recursive(choice.right), choice.firstBeforeSecond)
      case Consume(consume) => consume ^^ addState
      case Keyword(keyword, reserved) => core.grammar.Keyword(keyword, reserved) ^^ addState
      case Delimiter(keyword) => core.grammar.Delimiter(keyword) ^^ addState
      case many:Many => core.grammar.Many(recursive(many.inner)) ^^
        { case elements: Seq[Any] =>
          val WithMaps = elements.map({ case x:WithMap => x })
          WithMap(WithMaps.map(x => x.value), WithMaps.map(x => x.state).fold(Map.empty)((a,b) => a ++ b))
        }
      case mapGrammar: MapGrammar => core.grammar.MapGrammar(recursive(mapGrammar.inner),
          if (!mapGrammar.showMap)
            { case WithMap(value, state) => WithMap(mapGrammar.construct(value), state) }
          else
            mapGrammar.construct
        )
      case BiFailure => core.grammar.FailureG()
      case Print(document) => core.grammar.Produce(Unit) ^^ addState //TODO really want unit here?
      case Produce(value) => core.grammar.Produce(value) ^^ addState
      case As(inner, key) => recursive(inner) ^^
        { case WithMap(v, state) => WithMap(Unit, state ++ Map(key -> v))} //TODO really want unit here?
    }

    override def labelledLeave(inner: Grammar, partial: Grammar): Unit = partial.asInstanceOf[core.grammar.Labelled].inner = inner
  }
  
  def toGrammar(grammar: BiGrammar): Grammar = Observer.observe(grammar) ^^ { case WithMap(v,_) => v }
}
