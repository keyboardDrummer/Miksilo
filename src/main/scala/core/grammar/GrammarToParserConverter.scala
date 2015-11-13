package core.grammar


import scala.collection.mutable
import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}
import scala.util.parsing.input.CharArrayReader._

class GrammarToParserConverter extends JavaTokenParsers with PackratParsers {

  var keywords: Set[String] = Set.empty

  def whitespaceG: Parser[Any] = rep(
    whitespaceChar
      //| '/' ~ '*' ~ comment
      //| '/' ~ '/' ~ rep( chrExcept(EofCh, '\n') )
      //| '/' ~ '*' ~ failure("unclosed comment")
  )

  def whitespaceChar = elem("space char", ch => ch <= ' ' && ch != EofCh)
  protected def comment: Parser[Any] = (
    '*' ~ '/'  ^^ { case _ => ' '  }
      | chrExcept(EofCh) ~ comment
    )

  def chrExcept(cs: Char*) = elem("", ch => cs forall (ch != _))

  def convert(grammar: Grammar) : PackratParser[Any] = {
    val allGrammars: Set[Grammar] = grammar.getGrammars
    keywords ++= allGrammars.flatMap({
      case keyword: Keyword => if (keyword.reserved) Set(keyword.value) else Set.empty[String]
      case _ => Set.empty[String]
    })
    phrase(convertInner(grammar))
  }

  def convertInner(grammar: Grammar): PackratParser[Any] = {
    val map = new mutable.HashMap[Grammar, PackratParser[Any]]

    def helper(grammar: Grammar): PackratParser[Any] = {
      map.getOrElseUpdate(grammar, grammar.simplify match {
        case choice: Choice => helper(choice.left) ||| helper(choice.right)
        case sequence: Sequence => helper(sequence.first) ~ helper(sequence.second) ^^ {
          case l ~ r => new core.grammar.~(l, r)
        }
        case regexG: RegexG => regex(regexG.regex)
        case NumberG => wholeNumber
        case StringLiteral => stringLiteral ^^ (s => s.dropRight(1).drop(1))
        case many: Many => helper(many.inner).*
        case originalDelimiter: Delimiter => whitespaceG ~> literal(originalDelimiter.value)
        case originalKeyword: core.grammar.Keyword => literal(originalKeyword.value)
        case core.grammar.Identifier => ident.filter(identifier => !keywords.contains(identifier))
        case labelled: Labelled => helper(labelled.inner)
        case map: MapGrammar => helper(map.inner) ^^ map.map
        case produce: Produce => success(produce.result)
        case FailureG(message) => failure(message)
        case null => throw new RuntimeException("cannot convert empty grammar")
      })
    }

    helper(grammar)
  }

}
