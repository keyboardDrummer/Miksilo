package application.compilerCockpit

import core.bigrammar.BiGrammarToParser._
import core.bigrammar.grammars._
import core.bigrammar.textMate.BiGrammarToTextMate
import core.bigrammar.{BiGrammar, BiGrammarToParser}
import javax.swing.text.Segment
import org.fife.ui.rsyntaxtextarea.{TokenTypes, _}
import util.GraphBasics

import scala.collection.mutable
import scala.util.matching.Regex

case class MyToken(tokenType: Int, text: String)
class TokenMakerFromGrammar(grammar: BiGrammar) extends AbstractTokenMaker {

  val textMateScopeToToken: Map[String, Int] = Map(
    "string.quoted.single" -> TokenTypes.LITERAL_STRING_DOUBLE_QUOTE,
    "string.quoted.double" -> TokenTypes.LITERAL_STRING_DOUBLE_QUOTE,
    "comment.line.double-slash" -> TokenTypes.COMMENT_EOL,
    "comment.block" -> TokenTypes.COMMENT_MULTILINE)

  val parserBuilder: SequenceParserExtensions[Seq[MyToken]] = {
    val keywords: mutable.Set[String] = mutable.Set.empty
    val reachables = GraphBasics.traverseBreadth[BiGrammar](Seq(grammar), grammar => grammar.children,
      node => if (node.isInstanceOf[Colorize]) GraphBasics.SkipChildren else GraphBasics.Continue).toSet

    val tokenParsers: Set[BiGrammarToParser.Self[MyToken]] = reachables.collect({
      case keyword: Keyword if keyword.reserved =>
        keywords.add(keyword.value)
        literal(keyword.value, allowDrop = false) ^^ (s => MyToken(TokenTypes.RESERVED_WORD, s))
      case biLiteral: BiLiteral =>
        literal(biLiteral.value, allowDrop = false) ^^ (s => MyToken(TokenTypes.SEPARATOR, s))
      case identifier: Identifier => identifier.getParserBuilder(keywords) ^^ (s => MyToken(TokenTypes.IDENTIFIER, s))
      case NumberGrammar => wholeNumber ^^ (s => MyToken(TokenTypes.LITERAL_NUMBER_DECIMAL_INT, s)) //TODO should support other numbers as well.
      case StringLiteral =>
        stringLiteral ^^ (s => MyToken(TokenTypes.LITERAL_STRING_DOUBLE_QUOTE, s))
      case Colorize(inner, textMateScope) =>
        val regex = BiGrammarToTextMate.grammarToRegex(inner).get.r
        RegexParser(regex, regex.regex) ^^ (s => {
          MyToken(textMateScopeToToken(textMateScope), s)
        })
    })

    val whiteSpaceToken = RegexParser(new Regex("\\s+"), "whitespace") ^^ (s => MyToken(TokenTypes.WHITESPACE, s))
    val allTokenParsers = tokenParsers ++ Seq(whiteSpaceToken)

    val errorToken = RegexParser(new Regex("."), "anything") ^^ (s => MyToken(TokenTypes.ERROR_CHAR, s))
    choice(allTokenParsers.reduce((a, b) => a | b), errorToken, firstIsLonger = true).*
  }
  lazy val parser = parserBuilder.getWholeInputParser

  override def getWordsToHighlight: TokenMap = new TokenMap()

  override def getTokenList(text: Segment, initialTokenType: Int, startOffset: Int): Token = {

    resetTokenList()

    val resultOption: SingleParseResult[Seq[MyToken]] = parser.parse(new Reader(text.toString))
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
