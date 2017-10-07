package core.bigrammar

import core.document.{Document, WhiteSpace}
import core.grammar.{Identifier, NumberG}
import core.responsiveDocument.ResponsiveDocument

import scala.language.implicitConversions

trait GrammarDocumentWriter {

  def identifier: BiGrammar = new FromStringGrammar(Identifier)

  def number: BiGrammar = new FromGrammarWithToString(NumberG)

  def integer = number.map(afterParsing = (s: String) => Integer.parseInt(s), (i: Int) => Some(i.toString))

  def failure: BiGrammar = BiFailure()

  def value(value: Any): BiGrammar = new ValueGrammar(value)

  def keywordClass(value: String) = new Keyword(value, false, true)

  def space: BiGrammar = print(new WhiteSpace(1, 1))

  def keyword(word: String): BiGrammar = new Keyword(word)

  implicit def print(document: ResponsiveDocument): BiGrammar = new Print(document)

  implicit def print(document: Document): BiGrammar = new Print(document)

  implicit def stringToGrammar(value: String): BiGrammar =
    if (value.exists(c => Character.isLetterOrDigit(c))) //either exists or forall is a bit of an arbitrary choice. exists works better for syntax highlighting
      new Keyword(value)
    else new Delimiter(value)
}
