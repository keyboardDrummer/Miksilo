package application.compilerCockpit

import core.bigrammar.BiGrammarToParser._
import core.bigrammar.grammars._
import core.bigrammar.{BiGrammar, BiGrammarToParser}
import javax.swing.text.Segment
import org.fife.ui.rsyntaxtextarea.{TokenTypes, _}

import scala.collection.mutable
import scala.util.matching.Regex

case class MyToken(tokenType: Int, text: String)
class TokenMakerFromGrammar(grammar: BiGrammar) extends AbstractTokenMaker {

  val parser: EditorParserExtensions[Seq[MyToken]] = {
    val keywords: mutable.Set[String] = mutable.Set.empty
    val reachables = grammar.selfAndDescendants.toSet

    val tokenParsers: Set[BiGrammarToParser.EditorParser[MyToken]] = reachables.collect({
      case keyword: Keyword if keyword.reserved =>
        keywords.add(keyword.value)
        literal(keyword.value) ^^ (s => MyToken(TokenTypes.RESERVED_WORD, s))
      case delimiter: Delimiter => literal(delimiter.value) ^^ (s => MyToken(TokenTypes.SEPARATOR, s))
      case identifier: Identifier => identifier.getParser(keywords) ^^ (s => MyToken(TokenTypes.IDENTIFIER, s))
      case NumberGrammar => wholeNumber ^^ (s => MyToken(TokenTypes.LITERAL_NUMBER_DECIMAL_INT, s)) //TODO should support other numbers as well.
      case StringLiteral =>
        stringLiteral ^^ (s => MyToken(TokenTypes.LITERAL_STRING_DOUBLE_QUOTE, s))
      case Colorize(inner, _type) =>
        BiGrammarToParser.toParser(inner, keywords) ^^ (s => MyToken(_type, s.asInstanceOf[String]))
    })

    val whiteSpaceToken = regex(new Regex("\\s+")) ^^ (s => MyToken(TokenTypes.WHITESPACE, s))
    val allTokenParsers = tokenParsers ++ Seq(whiteSpaceToken)

    val errorToken = regex(new Regex(".")) ^^ (s => MyToken(TokenTypes.ERROR_CHAR, s))
    (allTokenParsers.reduce((a, b) => a | b) | errorToken).*
  }

  override def getWordsToHighlight: TokenMap = new TokenMap()

  override def getTokenList(text: Segment, initialTokenType: Int, startOffset: Int): Token = {

    resetTokenList()

    val resultOption: ParseResult[Seq[MyToken]] = parser.parseWholeInput(new BiGrammarToParser.Reader(text.toString))
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
