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

  val parser: BiGrammarToParser.Parser[Seq[MyToken]] = {
    var keywords: Set[String] = Set.empty
    val reachables = new RootGrammar(grammar).selfAndDescendants.map(p => p.value).toSet
    val tokenParsers: Set[BiGrammarToParser.Parser[MyToken]] = reachables.collect({
      case keyword: Keyword => {
        keywords += keyword.value
        literal(keyword.value) ^^ (s => MyToken(TokenTypes.RESERVED_WORD, s))
      }
      case delimiter: Delimiter => literal(delimiter.value) ^^ (s => MyToken(TokenTypes.SEPARATOR, s))
      case identifier:Identifier => identifier.getParser(keywords) ^^ (s => MyToken(TokenTypes.IDENTIFIER, s))
      case NumberG => wholeNumber ^^ (s => MyToken(TokenTypes.LITERAL_NUMBER_DECIMAL_INT, s))
      case StringLiteral => stringLiteral ^^ (s => MyToken(TokenTypes.LITERAL_STRING_DOUBLE_QUOTE, s))
      case regexGrammar: RegexGrammar => regex(regexGrammar.regex) ^^ (s => MyToken(TokenTypes.COMMENT_MULTILINE, s))
    })

    val whiteSpaceToken = regex(new Regex("\\s+")) ^^ (s => MyToken(TokenTypes.WHITESPACE, s))
    val errorToken = regex(new Regex(".")) ^^ (s => MyToken(TokenTypes.ERROR_CHAR, s))
    val allTokenParsers = Seq(whiteSpaceToken) ++ tokenParsers //++ Seq(whiteSpaceToken, errorToken)

    val tokenGrammar: BiGrammarToParser.Parser[Seq[MyToken]] = allTokenParsers.reduce((a, b) => a | b).*
    tokenGrammar //| errorToken
  }

  override def getWordsToHighlight: TokenMap = new TokenMap()

  override def getTokenList(text: Segment, initialTokenType: Int, startOffset: Int): Token = {

    resetTokenList()

    val charArrayReader = new CharArrayReader(text.toString.toCharArray)
    val resultOption: ParseResult[Seq[MyToken]] = parser(charArrayReader)
    var start = text.offset
    if (resultOption.isEmpty)
    {
      addToken(text, text.offset, start + text.length() - 1, TokenTypes.ERROR_CHAR, startOffset)
    } 
    else
    {
      val tokens = resultOption.get
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
