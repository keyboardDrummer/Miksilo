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
      //case delimiter: Delimiter => delimiter ^^ (s => MyToken(TokenTypes.SEPARATOR, s.asInstanceOf[String]))
      case Identifier => Identifier ^^ (s => MyToken(TokenTypes.IDENTIFIER, s.asInstanceOf[String]))
      //case regex: RegexG => regex ^^ (s => MyToken(TokenTypes.COMMENT_MULTILINE, s.asInstanceOf[String]))
    })

    val whiteSpaceToken = new RegexG(new Regex("\\s")) ^^ (s => MyToken(TokenTypes.WHITESPACE, s.asInstanceOf[String]))
    val errorToken = new RegexG(new Regex(".")) ^^ (s => MyToken(TokenTypes.ERROR_CHAR, s.asInstanceOf[String]))
    val allTokenParsers = tokenParsers ++ Seq(whiteSpaceToken, errorToken)

    val tokenGrammar = allTokenParsers.reduce((a, b) => a | b).*
    converter.convert(tokenGrammar)
  }

  override def getWordsToHighlight: TokenMap = new TokenMap()

  override def getTokenList(text: Segment, initialTokenType: Int, startOffset: Int): Token = {

    resetTokenList()

    val resultOption: converter.ParseResult[Any] = parser(new CharArrayReader(text.toString.toCharArray))
    if (resultOption.isEmpty)
    {
      addNullToken()  
    } 
    else
    {
      val tokens = resultOption.get.asInstanceOf[Seq[MyToken]]
      var start = text.offset
      for(token <- tokens)
      {
        val end = start + token.text.length - 1
        addToken(text, start, end, token.tokenType, startOffset)
        start = end + 1
      }
      addNullToken()
    }
    firstToken
  }


//  override def getWordsToHighlight: TokenMap = {
//    val reachables = grammar.getDescendantsAndSelf
//
//    val tokenMap = new TokenMap()
//
//    for(reachable <- reachables)
//    {
//      reachable match {
//        case keyword: Keyword => tokenMap.put(keyword.value, TokenTypes.RESERVED_WORD)
//        case _ =>
//      }
//    }
//    tokenMap
//  }

//  override def addToken(segment: Segment , start: Int, end: Int, tokenType: Int, startOffset: Int) {
//    // This assumes all keywords, etc. were parsed as "identifiers."
//    val newTokenType = if (tokenType == TokenTypes.IDENTIFIER) {
//      val value = wordsToHighlight.get(segment, start, end)
//      if (value != -1) {
//        value
//      }
//      else
//        tokenType
//    } else
//      tokenType
//    super.addToken(segment, start, end, newTokenType, startOffset)
//  }

  /*
  * Returns a list of tokens representing the given text.
  *
  * @param text The text to break into tokens.
  * @param startTokenType The token with which to start tokenizing.
  * @param startOffset The offset at which the line of tokens begins.
    * @return A linked list of tokens representing <code>text</code>.
    */
//  override def getTokenList(text: Segment, startTokenType: Int, startOffset: Int): Token = {
//
//    addTo
// resetTokenList();
//
//    val array = text.array;
//    val offset = text.offset;
//    val count = text.count;
//    val end = offset + count;
//
//    // Token starting offsets are always of the form:
//    // 'startOffset + (currentTokenStart-offset)', but since startOffset and
//    // offset are constant, tokens' starting positions become:
//    // 'newStartOffset+currentTokenStart'.
//    val newStartOffset = startOffset - offset;
//
//    var currentTokenStart = offset;
//    var currentTokenType  = startTokenType;
//
//    for (i <- offset.until(end)) {
//
//      val c = array(i)
//
//      currentTokenType match {
//
//        case TokenTypes.NULL =>
//
//        currentTokenStart = i;   // Starting a new token here.
//
//        c match {
//
//          case ' ' => currentTokenType = TokenTypes.WHITESPACE;
//          case '\t' => currentTokenType = TokenTypes.WHITESPACE
//          case '"' =>
//            currentTokenType = TokenTypes.LITERAL_STRING_DOUBLE_QUOTE
//
//          case '#' =>
//            currentTokenType = TokenTypes.COMMENT_EOL
//
//          case _ =>
//          if (RSyntaxUtilities.isDigit(c)) {
//            currentTokenType = TokenTypes.LITERAL_NUMBER_DECIMAL_INT
//          }
//          else if (RSyntaxUtilities.isLetter(c) || c=='/' || c=='_') {
//            currentTokenType = Token.IDENTIFIER;
//            break;
//          }
//
//          // Anything not currently handled - mark as an identifier
//          currentTokenType = Token.IDENTIFIER;
//          break;
//
//        } // End of switch (c).
//
//        break;
//
//        case Token.WHITESPACE:
//
//          switch (c) {
//
//          case ' ':
//          case '\t':
//          break;   // Still whitespace.
//
//          case '"':
//            addToken(text, currentTokenStart,i-1, Token.WHITESPACE, newStartOffset+currentTokenStart);
//          currentTokenStart = i;
//          currentTokenType = Token.LITERAL_STRING_DOUBLE_QUOTE;
//          break;
//
//          case '#':
//            addToken(text, currentTokenStart,i-1, Token.WHITESPACE, newStartOffset+currentTokenStart);
//          currentTokenStart = i;
//          currentTokenType = Token.COMMENT_EOL;
//          break;
//
//          default:   // Add the whitespace token and start anew.
//
//            addToken(text, currentTokenStart,i-1, Token.WHITESPACE, newStartOffset+currentTokenStart);
//          currentTokenStart = i;
//
//          if (RSyntaxUtilities.isDigit(c)) {
//            currentTokenType = Token.LITERAL_NUMBER_DECIMAL_INT;
//            break;
//          }
//          else if (RSyntaxUtilities.isLetter(c) || c=='/' || c=='_') {
//            currentTokenType = Token.IDENTIFIER;
//            break;
//          }
//
//          // Anything not currently handled - mark as identifier
//          currentTokenType = Token.IDENTIFIER;
//
//        } // End of switch (c).
//
//        break;
//
//        default: // Should never happen
//        case Token.IDENTIFIER:
//
//          switch (c) {
//
//          case ' ':
//          case '\t':
//          addToken(text, currentTokenStart,i-1, Token.IDENTIFIER, newStartOffset+currentTokenStart);
//          currentTokenStart = i;
//          currentTokenType = Token.WHITESPACE;
//          break;
//
//          case '"':
//            addToken(text, currentTokenStart,i-1, Token.IDENTIFIER, newStartOffset+currentTokenStart);
//          currentTokenStart = i;
//          currentTokenType = Token.LITERAL_STRING_DOUBLE_QUOTE;
//          break;
//
//          default:
//          if (RSyntaxUtilities.isLetterOrDigit(c) || c=='/' || c=='_') {
//            break;   // Still an identifier of some type.
//          }
//          // Otherwise, we're still an identifier (?).
//
//        } // End of switch (c).
//
//        break;
//
//        case Token.LITERAL_NUMBER_DECIMAL_INT:
//
//          switch (c) {
//
//          case ' ':
//          case '\t':
//          addToken(text, currentTokenStart,i-1, Token.LITERAL_NUMBER_DECIMAL_INT, newStartOffset+currentTokenStart);
//          currentTokenStart = i;
//          currentTokenType = Token.WHITESPACE;
//          break;
//
//          case '"':
//            addToken(text, currentTokenStart,i-1, Token.LITERAL_NUMBER_DECIMAL_INT, newStartOffset+currentTokenStart);
//          currentTokenStart = i;
//          currentTokenType = Token.LITERAL_STRING_DOUBLE_QUOTE;
//          break;
//
//          default:
//
//          if (RSyntaxUtilities.isDigit(c)) {
//            break;   // Still a literal number.
//          }
//
//          // Otherwise, remember this was a number and start over.
//          addToken(text, currentTokenStart,i-1, Token.LITERAL_NUMBER_DECIMAL_INT, newStartOffset+currentTokenStart);
//          i--;
//          currentTokenType = Token.NULL;
//
//        } // End of switch (c).
//
//        break;
//
//        case Token.COMMENT_EOL:
//          i = end - 1;
//        addToken(text, currentTokenStart,i, currentTokenType, newStartOffset+currentTokenStart);
//        // We need to set token type to null so at the bottom we don't add one more token.
//        currentTokenType = Token.NULL;
//        break;
//
//        case Token.LITERAL_STRING_DOUBLE_QUOTE:
//        if (c=='"') {
//          addToken(text, currentTokenStart,i, Token.LITERAL_STRING_DOUBLE_QUOTE, newStartOffset+currentTokenStart);
//          currentTokenType = Token.NULL;
//        }
//        break;
//
//      } // End of switch (currentTokenType).
//
//    } // End of for (int i=offset; i<end; i++).
//
//    switch (currentTokenType) {
//
//      // Remember what token type to begin the next line with.
//      case Token.LITERAL_STRING_DOUBLE_QUOTE:
//      addToken(text, currentTokenStart,end-1, currentTokenType, newStartOffset+currentTokenStart);
//      break;
//
//      // Do nothing if everything was okay.
//      case Token.NULL:
//        addNullToken();
//      break;
//
//      // All other token types don't continue to the next line...
//      default:
//        addToken(text, currentTokenStart,end-1, currentTokenType, newStartOffset+currentTokenStart);
//      addNullToken();
//
//    }
//
//    // Return the first token in our linked list.
//    return firstToken;
//
//  }
}
