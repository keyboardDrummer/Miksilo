package core.bigrammar

import core.bigrammar.grammars._
import core.document.{Document, WhiteSpace}
import core.responsiveDocument.ResponsiveDocument

import scala.language.implicitConversions

object BiGrammarWriter extends BiGrammarWriter
trait BiGrammarWriter {

  def identifier: BiGrammar = Identifier()

  def number: BiGrammar = NumberG

  def integer = number ^^ (
    (s: Any) => Integer.parseInt(s.asInstanceOf[String]),
    (i: Any) => Some(i).filter(i => i.isInstanceOf[Int]).map(i => i.toString)
  )

  def failure: BiGrammar = BiFailure()

  def value(value: Any): BiGrammar = ValueGrammar(value)

  def keywordClass(value: String) = Keyword(value, false, true)

  def space: BiGrammar = print(WhiteSpace(1, 1))

  def keyword(word: String): BiGrammar = new Keyword(word)

  implicit def print(document: ResponsiveDocument): BiGrammar = new Print(document)

  implicit def print(document: Document): BiGrammar = new Print(document)

  implicit def stringToGrammar(value: String): BiGrammar =
    if (value.exists(c => Character.isLetterOrDigit(c))) //either exists or forall is a bit of an arbitrary choice. exists works better for syntax highlighting
      new Keyword(value)
    else
      new Delimiter(value)
}
