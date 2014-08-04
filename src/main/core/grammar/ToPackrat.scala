package core.grammar

import scala.collection.mutable
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

class ToPackrat extends StandardTokenParsers with PackratParsers {
  def convert(grammar: Grammar): PackratParser[Any] = {
    val map = new mutable.HashMap[Grammar,PackratParser[Any]]

    def helper(grammar: Grammar) : PackratParser[Any] = {
      map.getOrElseUpdate(grammar, grammar match {
        case sequence: Sequence => helper(sequence.first) ~ helper(sequence.second) ^^ {
          case l ~ r => new seqr(l, r)
        }
        case choice: Choice => helper(choice.left) ||| helper(choice.right)
        case NumberG => numericLit
        case many: Many => helper(many.inner).*
        case originalDelimiter: Delimiter => keyword(originalDelimiter.value)
        case originalKeyword: Keyword => keyword(originalKeyword.value)
        case Identifier => ident
        case SuccessG => success[Any](null)
        case labelled: Labelled => helper(labelled.inner)
        case map: MapGrammar => helper(map.inner) ^^ map.forward
        case produce: Produce => success(produce.result)
        case FailureG => failure("fail")
        case null => throw new RuntimeException("cannot convert empty grammar")
      })
    }

    helper(grammar)
  }

}
