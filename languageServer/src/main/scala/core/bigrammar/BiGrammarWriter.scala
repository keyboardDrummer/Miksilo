package core.bigrammar

import core.bigrammar.grammars._
import core.document.{Document, WhiteSpace}
import core.responsiveDocument.ResponsiveDocument

import scala.language.implicitConversions
import scala.util.matching.Regex

object BiGrammarWriter extends BiGrammarWriter

trait BiGrammarWriter {

  def regexGrammar(regex: Regex, name: String): BiGrammar = RegexGrammar(regex, name)
  def identifier = getIdentifier()
  def getIdentifier(verifyWhenPrinting: Boolean = false): BiGrammar = Identifier(verifyWhenPrinting)

  def stringLiteral: BiGrammar = StringLiteral

  def number: BiGrammar = NumberGrammar

  def integer: BiGrammar = number.map[String, Int](
    s => Integer.parseInt(s),
    i => i.toString
  )

  def leftRight(left: BiGrammar, right: BiGrammar, bijective: SequenceBijective) =
    new BiSequence(left, right, bijective, true)

  def topBottom(top: BiGrammar, bottom: BiGrammar, bijective: SequenceBijective) =
    new BiSequence(top, bottom, bijective, false)

  def failure: BiGrammar = BiFailure()

  def value(value: Any): BiGrammar = ValueGrammar(value)

  def keywordClass(value: String): BiGrammar = Keyword(value, reserved = false, verifyWhenPrinting = true)

  def printSpace: BiGrammar = print(WhiteSpace(1, 1))

  def keywordGrammar(word: String): BiGrammar = Keyword(word)

  implicit def print(document: ResponsiveDocument): BiGrammar = Print(document)

  implicit def print(document: Document): BiGrammar = Print(document)

  implicit def implicitStringToGrammar(value: String): BiGrammar = stringToGrammar(value)

  def stringToGrammar(value: String, reserved: Boolean = true): BiGrammar = {
    if (value.contains(' '))
      throw new RuntimeException(s"Can't implicitly convert $value to BiGrammar because it contains a space")

    val count = value.count(c => Character.isLetterOrDigit(c) || c == '_')
    if (count == value.length)
      Keyword(value, reserved)
    else if (count == 0)
      BiLiteral(value)
    else
      throw new RuntimeException(s"Can't implicitly convert $value to BiGrammar because of mixed character types")
  }
}
