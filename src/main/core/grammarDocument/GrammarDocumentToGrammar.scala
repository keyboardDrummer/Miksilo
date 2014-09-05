package core.grammarDocument

import core.grammar.Grammar
import scala.collection.mutable

object GrammarDocumentToGrammar {
  def toGrammar(grammarDocument: GrammarDocument): Grammar = {
    val closed = new mutable.HashMap[GrammarDocument, Grammar]()

    def helper(grammarDocument: GrammarDocument): Grammar = {
      closed.getOrElseUpdate(grammarDocument, grammarDocument.simplify match {
        case Sequence(first, second) => core.grammar.Sequence(helper(first), helper(second))
        case Choice(first, second) => core.grammar.Choice(helper(first), helper(second))
        case Consume(consume) => consume
        case Keyword(keyword) => core.grammar.Keyword(keyword)
        case Delimiter(keyword) => core.grammar.Delimiter(keyword)
        case labelled: Labelled =>
          val result = new core.grammar.Labelled(labelled.name, null)
          closed.put(grammarDocument, result)
          result.inner = helper(labelled.inner)
          result
        case Many(inner) => core.grammar.Many(helper(inner))
        case MapGrammar(inner, construct, _) => core.grammar.MapGrammar(helper(inner), construct)
        case TopBottom(top, bottom) => core.grammar.Sequence(helper(top), helper(bottom))
        case FailureG => core.grammar.FailureG
        case WhiteSpace(_,_) => core.grammar.Produce(Unit)
        case Produce(value) => core.grammar.Produce(value)
      })
    }

    helper(grammarDocument)
  }
}
