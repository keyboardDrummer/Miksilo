package core.bigrammar

import core.bigrammar.BiGrammar.State
import core.bigrammar.grammars._
import core.deltas.node.GrammarKey
import core.grammar.{Grammar, ~}

case class WithMapG[T](value: T, map: Map[Any,Any]) {}

object BiGrammarToGrammar {
  type WithMap = WithMapG[Any]

  type Result = StateFull[WithMap]

  def valueToResult(value: Any): Result = StateFull.ret(WithMapG(value, Map.empty))

  //noinspection ZeroIndexToHead
  object Observer extends BiGrammarObserver[Grammar] {
    override def getReference(name: GrammarKey): Grammar = new core.grammar.Labelled(name)

    override def handleGrammar(self: BiGrammar, children: Seq[Grammar], recursive: (BiGrammar) => Grammar): Grammar = {
      self match {
        case _: SequenceLike =>
          core.grammar.Sequence(children(0), children(1)) ^^ { untyped =>
            val ~(firstResult: Result, secondResult: Result) = untyped.asInstanceOf[~[Result,Result]]
            new StateFull[WithMap] {
              override def run(state: State) = {
                val firstMap = firstResult(state)
                val secondMap = secondResult(firstMap._1)
                (secondMap._1, WithMapG(core.grammar.~(firstMap._2.value, secondMap._2.value), firstMap._2.map ++ secondMap._2.map))
              }
            }
          }
        case choice: Choice => core.grammar.Choice(children(0), children(1), choice.firstBeforeSecond)
        case custom: CustomGrammarWithoutChildren => custom.getGrammar ^^ valueToResult
        case custom: CustomGrammar => custom.createGrammar(children, recursive)
        case Keyword(keyword, reserved, _) => core.grammar.Keyword(keyword, reserved) ^^ valueToResult
        case Delimiter(keyword) => core.grammar.Delimiter(keyword) ^^ valueToResult
        case _: Many => core.grammar.Many(children.head) ^^ { untyped =>
          val elements = untyped.asInstanceOf[Seq[Result]]
          new StateFull[WithMap] {
            override def run(initialState: State) = {
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
            }
          }
        }
        case mapGrammar: MapGrammarWithMap => core.grammar.MapGrammar(children.head,
          result => result.asInstanceOf[Result].map(x => mapGrammar.construct(x)))

        case BiFailure(message) => core.grammar.FailureG(message)
        case Print(_) => core.grammar.Produce(Unit) ^^ valueToResult //TODO really want unit here?
        case ValueGrammar(value) => core.grammar.Produce(value) ^^ valueToResult
        case As(inner, key) => children.head ^^
          (result => result.asInstanceOf[Result].map { case WithMapG(v, state) => WithMapG(inner, state + (key -> v)) })
        case labelled: Labelled => new core.grammar.Labelled(labelled.name, children.head)
      }
    }

    override def setReference(inner: Grammar, partial: Grammar): Unit = partial.asInstanceOf[core.grammar.Labelled].inner = inner
  }

  
  def toGrammar(grammar: BiGrammar): Grammar = Observer.observe(grammar) ^^ (r => r.asInstanceOf[Result](Map.empty[Any,Any])._2.value)
}
