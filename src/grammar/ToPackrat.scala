package grammar

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.collection.mutable

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
        case originalKeyword: Keyword => keyword(originalKeyword.value)
        case Identifier => ident
        case SuccessG => success[Any](null)
        case labelled: Labelled => helper(labelled.inner)
        case map: MapGrammar => helper(map.inner) ^^ map.forward
        case layz: Lazy => helper(layz.getInner)
        case produce: Produce => success(produce.result)
        case FailureG => failure("fail")
      })
    }

    helper(grammar)
  }

}
