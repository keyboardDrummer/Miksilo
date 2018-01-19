package application.compilerCockpit

import javax.swing.text.Segment

import core.bigrammar.BiGrammarToParser._
import core.bigrammar.grammars._
import core.bigrammar.{BiGrammar, BiGrammarToParser}
import org.fife.ui.rsyntaxtextarea.{TokenTypes, _}

import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.parsing.input.CharArrayReader

case class MyToken(tokenType: Int, text: String)
class TokenMakerFromGrammar(grammar: BiGrammar) extends AbstractTokenMaker {


  val parser: BiGrammarToParser.Parser[Seq[MyToken]] = {
    val keywords: mutable.Set[String] = mutable.Set.empty
    val reachables = grammar.selfAndDescendants.toSet

    val tokenParsers: Set[BiGrammarToParser.Parser[MyToken]] = reachables.collect({
      case keyword: Keyword if keyword.reserved =>
        keywords.add(keyword.value)
        literal(keyword.value) ^^ (s => MyToken(TokenTypes.RESERVED_WORD, s))
      case delimiter: Delimiter => literal(delimiter.value) ^^ (s => MyToken(TokenTypes.SEPARATOR, s))
      case identifier: Identifier => identifier.getParser(keywords) ^^ (s => MyToken(TokenTypes.IDENTIFIER, s))
      case NumberGrammar => wholeNumber ^^ (s => MyToken(TokenTypes.LITERAL_NUMBER_DECIMAL_INT, s)) //TODO shoud support other numbers as well.
      case StringLiteral =>
        stringLiteral ^^ (s => MyToken(TokenTypes.LITERAL_STRING_DOUBLE_QUOTE, s))
      case TokenColor(inner, _type) =>
        BiGrammarToParser.toParser(inner, keywords) ^^ (s => MyToken(_type, s.asInstanceOf[String]))
    })

    val whiteSpaceToken = regex(new Regex("\\s+")) ^^ (s => MyToken(TokenTypes.WHITESPACE, s))
    val errorToken = regex(new Regex(".")) ^^ (s => MyToken(TokenTypes.ERROR_CHAR, s))
    val allTokenParsers = tokenParsers ++ Seq(whiteSpaceToken)

    val tokenGrammar = (allTokenParsers.reduce((a, b) => a | b) | errorToken).*
    phrase(tokenGrammar)
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
