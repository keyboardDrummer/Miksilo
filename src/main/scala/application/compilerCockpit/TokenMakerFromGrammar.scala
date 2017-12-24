package application.compilerCockpit

import javax.swing.text.Segment

import core.bigrammar.{BiGrammar, BiGrammarToParser, RootGrammar}
import core.bigrammar.grammars.{RegexGrammar, _}
import org.fife.ui.rsyntaxtextarea._

import scala.util.matching.Regex
import scala.util.parsing.input.CharArrayReader
import BiGrammarToParser._

class TokenMakerFromGrammar(grammar: BiGrammar) extends AbstractTokenMaker {

  case class MyToken(tokenType: Int, text: String)

  val parser = getParser

  def getParser: BiGrammarToParser.Parser[Any] = {
    val reachables = new RootGrammar(grammar).selfAndDescendants.map(p => p.value)
    val tokenParsers: Seq[BiGrammarToParser.Parser[MyToken]] = reachables.collect({
      case keyword: Keyword => literal(keyword.value) ^^ (s => MyToken(TokenTypes.RESERVED_WORD, s))
      case delimiter: Delimiter => literal(delimiter.value) ^^ (s => MyToken(TokenTypes.SEPARATOR, s))
      case _:Identifier => ident ^^ (s => MyToken(TokenTypes.IDENTIFIER, s))
      case NumberG => wholeNumber ^^ (s => MyToken(TokenTypes.LITERAL_NUMBER_DECIMAL_INT, s))
      case StringLiteral => stringLiteral ^^ (s => MyToken(TokenTypes.LITERAL_STRING_DOUBLE_QUOTE, s))
      case regexGrammar: RegexGrammar => regex(regexGrammar.regex) ^^ (s => MyToken(TokenTypes.COMMENT_MULTILINE, s))
    })

    val whiteSpaceToken = regex(new Regex("\\s+")) ^^ (s => MyToken(TokenTypes.WHITESPACE, s))
    val errorToken = regex(new Regex(".")) ^^ (s => MyToken(TokenTypes.ERROR_CHAR, s))
    val allTokenParsers = tokenParsers ++ Seq(whiteSpaceToken)

    val tokenGrammar = allTokenParsers.reduce((a, b) => a | b).*
    tokenGrammar | errorToken
  }

  override def getWordsToHighlight: TokenMap = new TokenMap()

  override def getTokenList(text: Segment, initialTokenType: Int, startOffset: Int): Token = {

    resetTokenList()

    val charArrayReader = new CharArrayReader(text.toString.toCharArray)
    val resultOption: ParseResult[Any] = parser(charArrayReader)
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
