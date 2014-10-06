package core.grammarDocument

import core.grammar.Grammar

import scala.collection.mutable

object GrammarDocumentToGrammar {
  def toGrammar(grammar: BiGrammar): Grammar = {
    val closed = new mutable.HashMap[BiGrammar, Grammar]()

    def toGrammarCached(grammar: BiGrammar): Grammar = {
      closed.getOrElseUpdate(grammar, grammar.simplify match {
        case Sequence(first, second) => core.grammar.Sequence(toGrammarCached(first), toGrammarCached(second))
        case Choice(first, second) => core.grammar.Choice(toGrammarCached(first), toGrammarCached(second))
        case Consume(consume) => consume
        case Keyword(keyword) => core.grammar.Keyword(keyword)
        case Delimiter(keyword) => core.grammar.Delimiter(keyword)
        case labelled: Labelled =>
          val result = new core.grammar.Labelled(labelled.name, null)
          closed.put(grammar, result)
          result.inner = toGrammarCached(labelled.inner)
          result
        case Many(inner) => core.grammar.Many(toGrammarCached(inner))
        case MapGrammar(inner, construct, _) => core.grammar.MapGrammar(toGrammarCached(inner), construct)
        case TopBottom(top, bottom) => core.grammar.Sequence(toGrammarCached(top), toGrammarCached(bottom))
        case FailureGD => core.grammar.FailureG
        case Print(_) => core.grammar.Produce(Unit)
        case Produce(value) => core.grammar.Produce(value)
      })
    }

    toGrammarCached(grammar)
  }
}
