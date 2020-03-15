package playground.application.compilerCockpit

import core.bigrammar.grammars._
import core.bigrammar.{BiGrammar, BiGrammarToParser}
import core.textMate.GenerateTextMateGrammar
import javax.swing.text.Segment
import org.fife.ui.rsyntaxtextarea.{TokenTypes, _}
import util.GraphBasics

import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.parsing.combinator.{JavaTokenParsers, Parsers, RegexParsers}
import scala.util.parsing.input.{CharArrayReader, CharSequenceReader}

case class MyToken(tokenType: Int, text: String)
class TokenMakerFromGrammar(grammar: BiGrammar) extends AbstractTokenMaker
  with Parsers with RegexParsers with JavaTokenParsers {

  override protected val whiteSpace = "".r

  val textMateScopeToToken: Map[String, Int] = Map(
    "string.quoted.single" -> TokenTypes.LITERAL_STRING_DOUBLE_QUOTE,
    "string.quoted.double" -> TokenTypes.LITERAL_STRING_DOUBLE_QUOTE,
    "comment.line.double-slash" -> TokenTypes.COMMENT_EOL,
    "comment.block" -> TokenTypes.COMMENT_MULTILINE)

  val parser: Parser[Seq[MyToken]] = {
    val keywords: mutable.Set[String] = mutable.Set.empty
    val reachables = GraphBasics.traverseBreadth[BiGrammar](Seq(grammar), grammar => grammar.children,
      node => if (node.isInstanceOf[Colorize]) GraphBasics.SkipChildren else GraphBasics.Continue).toSet

    val tokenParsers: Set[Parser[MyToken]] = reachables.collect({
      case keyword: Keyword if keyword.reserved =>
        keywords.add(keyword.value)
        literal(keyword.value) ^^ (s => MyToken(TokenTypes.RESERVED_WORD, s))
      case delimiter: Delimiter =>
        literal(delimiter.value) ^^ (s => MyToken(TokenTypes.SEPARATOR, s))
      case _: Identifier => ident.filter(s => !keywords.contains(s)) ^^ (s => MyToken(TokenTypes.IDENTIFIER, s))
      case NumberGrammar =>
        wholeNumber ^^ (s => MyToken(TokenTypes.LITERAL_NUMBER_DECIMAL_INT, s)) //TODO should support other numbers as well.
      case StringLiteral =>
        stringLiteral ^^ (s => MyToken(TokenTypes.LITERAL_STRING_DOUBLE_QUOTE, s))
      case Colorize(inner, textMateScope) =>
        val theRegex = GenerateTextMateGrammar.grammarToRegex(BiGrammarToParser)(BiGrammarToParser.toParserBuilder(inner)).get.r
        regex(theRegex) ^^ (s => {
          MyToken(textMateScopeToToken(textMateScope), s)
        })
    })

    val whiteSpaceToken = regex(new Regex("\\s+")) ^^ (s => MyToken(TokenTypes.WHITESPACE, s))
    val allTokenParsers: Set[Parser[MyToken]] = tokenParsers + whiteSpaceToken

    val errorToken = regex(new Regex(".")) ^^ (s => MyToken(TokenTypes.ERROR_CHAR, s))
    phrase((allTokenParsers.reduce((a, b) => a | b) | errorToken).*)
  }

  override def getWordsToHighlight: TokenMap = new TokenMap()

  override def getTokenList(text: Segment, initialTokenType: Int, startOffset: Int): Token = {

    resetTokenList()

    val resultOption = parser.apply(new CharSequenceReader(text))
    var start = text.offset
    if (resultOption.successful) {
      val tokens = resultOption.get
      for (token <- tokens) {
        val end = start + token.text.length - 1
        addToken(text, start, end, token.tokenType, start - text.offset + startOffset)
        start = end + 1
      }
    } else {
      addToken(text, text.offset, start + text.length() - 1, TokenTypes.ERROR_CHAR, startOffset)
    }
    addNullToken()
    firstToken
  }
}
