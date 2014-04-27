package grammar

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

trait ToPackrat extends StandardTokenParsers with PackratParsers {
  def convert(grammar: Grammar): PackratParser[Any] = grammar match {
    case sequence: Sequence => convert(sequence.first) ~ convert(sequence.second) ^^ {
      case l ~ r => new seqr(l, r)
    }
    case choice: Choice => convert(choice.left) | convert(choice.right)
    case many: Many => convert(many.inner).*
    case originalKeyword: Keyword => keyword(originalKeyword.value)
    case Identifier => ident
    case labelled: Labelled => convert(labelled.inner)
    case map: MapGrammar => convert(map.inner) ^^ map.forward
    case produce: Produce => success(produce.result)
    case FailureG => failure("fail")
  }
}
