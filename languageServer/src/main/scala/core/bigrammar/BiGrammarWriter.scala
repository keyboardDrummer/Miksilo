package core.bigrammar

import core.bigrammar.grammars._
import core.document.{Document, WhiteSpace}
import core.responsiveDocument.ResponsiveDocument

import scala.language.implicitConversions
import scala.util.Try
import scala.util.matching.Regex

object BiGrammarWriter extends BiGrammarWriter

trait BiGrammarWriter {

  def regexGrammar(regex: Regex, name: String): BiGrammar[String] = RegexGrammar(regex, name)
  def identifier = getIdentifier()
  def getIdentifier(verifyWhenPrinting: Boolean = false): BiGrammar[String] = Identifier(verifyWhenPrinting)

  def stringLiteral: BiGrammar[String] = StringLiteral

  def number: BiGrammar[String] = NumberGrammar

  def integer: BiGrammar[Int] = new MapGrammar[String, Int](number,
    s => Try(Integer.parseInt(s)).fold(e => Left(e.toString), x => Right(x)),
    i => Some(i.toString)
  )

  def long: BiGrammar[Long] = number.map[Long](
    s => s.toLong,
    i => i.toString
  )

  def leftRight[Left, Right, Result](left: BiGrammar[Left], right: BiGrammar[Right], bijective: SequenceBijective[Left, Right, Result]) =
    new BiSequence(left, right, bijective, true)

  def topBottom[Left, Right, Result](top: BiGrammar[Left], bottom: BiGrammar[Right], bijective: SequenceBijective[Left, Right, Result]) =
    new BiSequence(top, bottom, bijective, false)

  def failure: BiGrammar[Nothing] = BiFailure()

  def value[T](value: T): BiGrammar[T] = ValueGrammar(value)

  def printSpace: BiGrammar[Unit] = print(WhiteSpace(1, 1))

  def keywordGrammar(word: String): BiGrammar[Unit] = Keyword(word)

  implicit def print(document: ResponsiveDocument): BiGrammar[Unit] = Print(document)

  implicit def print(document: Document): BiGrammar[Unit] = Print(document)

  implicit def implicitStringToGrammar(value: String): BiGrammar[Unit] = stringToGrammar(value)

  def stringToGrammar(value: String, reserved: Boolean = true): BiGrammar[Unit] = {
    if (value.contains(' '))
      throw new RuntimeException(s"Can't implicitly convert $value to BiGrammar because it contains a space")

    val count = value.count(c => Character.isLetterOrDigit(c) || c == '_')
    if (count == value.length)
      Keyword(value, reserved)
    else if (count == 0)
      Delimiter(value)
    else
      throw new RuntimeException(s"Can't implicitly convert $value to BiGrammar because of mixed character types")
  }
}
