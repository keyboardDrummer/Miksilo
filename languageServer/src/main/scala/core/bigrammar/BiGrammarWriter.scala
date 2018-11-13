package core.bigrammar

import core.bigrammar.grammars._
import core.document.{Document, WhiteSpace}
import core.responsiveDocument.ResponsiveDocument

import scala.language.implicitConversions

object BiGrammarWriter extends BiGrammarWriter

trait BiGrammarWriter {

  def identifier: BiGrammar = Identifier()

  def number: BiGrammar = NumberGrammar

  def integer: BiGrammar = number.map[String, Int](
    s => Integer.parseInt(s),
    i => i.toString
  )

  def failure: BiGrammar = BiFailure()

  def value(value: Any): BiGrammar = ValueGrammar(value)

  def keywordClass(value: String) = Keyword(value, reserved = false, verifyWhenPrinting = true)

  def printSpace: BiGrammar = print(WhiteSpace(1, 1))

  def keyword(word: String): BiGrammar = Keyword(word)

  implicit def print(document: ResponsiveDocument): BiGrammar = Print(document)

  implicit def print(document: Document): BiGrammar = Print(document)

  implicit def stringToGrammar(value: String): BiGrammar = {
    if (value.contains(' '))
      throw new RuntimeException(s"Can't implicitly convert $value to BiGrammar because it contains a space")

    val count = value.count(c => Character.isLetterOrDigit(c) || c == '_')
    if (count == value.length)
      Keyword(value)
    else if (count == 0)
      Delimiter(value)
    else
      throw new RuntimeException(s"Can't implicitly convert $value to BiGrammar because of mixed character types")
  }
}
