package application.compilerCockpit

import javax.swing.text.Segment

import core.grammar._
import org.fife.ui.rsyntaxtextarea._

import scala.util.matching.Regex
import scala.util.parsing.input.CharArrayReader

class TokenMakerFromGrammar(grammar: Grammar) extends AbstractTokenMaker {

  class Converter extends GrammarToParserConverter
  {
    override def skipWhitespace = false
  }

  case class MyToken(tokenType: Int, text: String)
  val converter = new Converter()

  val parser = getParser

  def getParser: converter.Parser[Any] = {
    val reachables = grammar.getGrammars
    val tokenParsers = reachables.collect({
      case keyword: Keyword => keyword ^^ (s => MyToken(TokenTypes.RESERVED_WORD, s.asInstanceOf[String]))
      case delimiter: Delimiter => Keyword(delimiter.value, false) ^^ (s => MyToken(TokenTypes.SEPARATOR, s.asInstanceOf[String]))
      case Identifier => Identifier ^^ (s => MyToken(TokenTypes.IDENTIFIER, s.asInstanceOf[String]))
      case NumberG => NumberG ^^ (s => MyToken(TokenTypes.LITERAL_NUMBER_DECIMAL_INT, s.asInstanceOf[String]))
      case StringLiteral => StringLiteral ^^ (s => MyToken(TokenTypes.LITERAL_STRING_DOUBLE_QUOTE, '"' + s.asInstanceOf[String] + '"'))
      case regex: RegexG => regex ^^ (s => MyToken(TokenTypes.COMMENT_MULTILINE, s.asInstanceOf[String]))
    })

    val whiteSpaceToken = RegexG(new Regex("\\s+")) ^^ (s => MyToken(TokenTypes.WHITESPACE, s.asInstanceOf[String]))
    val errorToken = new RegexG(new Regex(".")) ^^ (s => MyToken(TokenTypes.ERROR_CHAR, s.asInstanceOf[String]))
    val allTokenParsers = tokenParsers ++ Seq(whiteSpaceToken)

    val tokenGrammar = allTokenParsers.reduce((a, b) => new Choice(a, b)).*
    converter.convert(new Choice(tokenGrammar, errorToken, true))
  }

  override def getWordsToHighlight: TokenMap = new TokenMap()

  override def getTokenList(text: Segment, initialTokenType: Int, startOffset: Int): Token = {

    resetTokenList()

    val charArrayReader = new CharArrayReader(text.toString.toCharArray)
    val resultOption: converter.ParseResult[Any] = parser(charArrayReader)
    var start = text.offset
    if (resultOption.isEmpty)
    {
      addToken(text, text.offset, start + text.length() - 1, TokenTypes.ERROR_CHAR, startOffset)
    } 
    else
    {
      val tokens = resultOption.get.asInstanceOf[Seq[MyToken]]
      for(token <- tokens)
      {
        val end = start + token.text.length - 1
        addToken(text, start, end, token.tokenType, start - text.offset + startOffset)
        start = end + 1
      }
    }
    addNullToken()
    firstToken
  }
}
