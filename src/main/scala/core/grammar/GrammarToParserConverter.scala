package core.grammar


import core.bigrammar.grammars.Keyword

import scala.collection.mutable
import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}
import scala.util.parsing.input.CharArrayReader._

object GrammarToParserConverter extends GrammarToParserConverter
class GrammarToParserConverter extends JavaTokenParsers with PackratParsers {

  override val whiteSpace = "".r

  var keywords: Set[String] = Set.empty

  def whitespaceG: Parser[Any] = rep(
    whitespaceChar
      //| '/' ~ '*' ~ comment
      //| '/' ~ '/' ~ rep( chrExcept(EofCh, '\n') )
      //| '/' ~ '*' ~ failure("unclosed comment")
  )

  def whitespaceChar = elem("space char", ch => ch <= ' ' && ch != EofCh)

  def convert(grammar: Grammar) : PackratParser[Any] = {
    val allGrammars: Set[Grammar] = grammar.getGrammars
    keywords ++= allGrammars.flatMap({
      case keyword: Keyword => if (keyword.reserved) Set(keyword.value) else Set.empty[String]
      case _ => Set.empty[String]
    })
    phrase(convertInner(grammar))
  }

  /*
  This function does not loop because the implicit calls that wraps parsers into packrat parsers take their argument lazily.
   */
  def convertInner(grammar: Grammar): PackratParser[Any] = {
    val map = new mutable.HashMap[Grammar, PackratParser[Any]]

    def helper(grammar: Grammar): PackratParser[Any] = {
      map.getOrElseUpdate(grammar, grammar.expand match {
        case choice: Choice => if (choice.firstBeforeSecond)
          helper(choice.left) | helper(choice.right)
          else helper(choice.left) ||| helper(choice.right)
        case sequence: Sequence => helper(sequence.first) ~ helper(sequence.second) ^^ {
          case l ~ r => new core.grammar.~(l, r)
        }
        case regexG: RegexG => regex(regexG.regex)
        case many: Many => helper(many.inner).*
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
