package core.bigrammar

import core.grammar.{Grammar, ~}

case class WithMapG[T](value: T, map: Map[Any,Any]) {}


object BiGrammarToGrammar {
  type WithMap = WithMapG[Any]
  type State = Map[Any, Any]

  case class StateM[T](run: State => (State, T)) {
    def map(f: T => T): StateM[T] = StateM((state: State) => {
      val (newState, value) = run(state)
      (newState, f(value))
    })

    def apply(state: State) = run(state)
  }
  type Result = StateM[WithMap]

  def valueToResult(value: Any): Result = StateM((state: State) => (state, WithMapG(value, Map.empty)))
  object Observer extends BiGrammarObserver[Grammar] {
    override def labelledEnter(name: AnyRef): Grammar = new core.grammar.Labelled(name)

    override def handleGrammar(self: BiGrammar, recursive: (BiGrammar) => Grammar): Grammar = {
      val size = Thread.currentThread().getStackTrace.size
      self match {
        case sequence: SequenceLike =>
          core.grammar.Sequence(recursive(sequence.first), recursive(sequence.second)) ^^ { case ~(firstResult: Result, secondResult: Result) => StateM((state: State) => {
            val firstMap = firstResult(state)
            val secondMap = secondResult(firstMap._1)
            (secondMap._1, WithMapG(core.grammar.~(firstMap._2.value, secondMap._2.value), firstMap._2.map ++ secondMap._2.map))
          })
          }
        case choice: Choice => core.grammar.Choice(recursive(choice.left), recursive(choice.right), choice.firstBeforeSecond)
        case custom: CustomGrammar => custom.getGrammar ^^ valueToResult
        case custom: SuperCustomGrammar => custom.createGrammar(recursive)
        case Keyword(keyword, reserved, _) => core.grammar.Keyword(keyword, reserved) ^^ valueToResult
        case Delimiter(keyword) => core.grammar.Delimiter(keyword) ^^ valueToResult
        case many: Many => core.grammar.Many(recursive(many.inner)) ^^ {
          case elements: Seq[Result] => StateM((initialState: State) => {
            var state = initialState
            var withMapState = Map.empty[Any, Any]
            var result = List.empty[Any]
            elements.foreach(r => {
              val withMap = r(state)
              state = withMap._1
              withMapState = withMapState ++ withMap._2.map
              result ::= withMap._2.value
            })
            (state, WithMapG(result.reverse, withMapState))
          })
        }
        case mapGrammar: MapGrammar => core.grammar.MapGrammar(recursive(mapGrammar.inner),
          if (!mapGrammar.showMap)
            result => result.asInstanceOf[Result].map { case WithMapG(value, state) => WithMapG(mapGrammar.construct(value), state) }
          else
            result => result.asInstanceOf[Result].map(x => mapGrammar.construct(x).asInstanceOf[WithMap])
        )
        case BiFailure(message) => core.grammar.FailureG(message)
        case Print(_) => core.grammar.Produce(Unit) ^^ valueToResult //TODO really want unit here?
        case ValueGrammar(value) => core.grammar.Produce(value) ^^ valueToResult
        case As(inner, key) => recursive(inner) ^^
          (result => result.asInstanceOf[Result].map { case WithMapG(v, state) => WithMapG(inner, state + (key -> v)) })
      }
    }

    override def labelledLeave(inner: Grammar, partial: Grammar): Unit = partial.asInstanceOf[core.grammar.Labelled].inner = inner
  }

  
  def toGrammar(grammar: BiGrammar): Grammar = Observer.observe(grammar) ^^ (r => r.asInstanceOf[Result](Map.empty[Any,Any])._2.value)
}
