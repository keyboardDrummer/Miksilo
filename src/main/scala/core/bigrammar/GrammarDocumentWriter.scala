package core.bigrammar

import core.document.{Document, WhiteSpace}
import core.grammar.{Grammar, Identifier, NumberG}
import core.responsiveDocument.ResponsiveDocument

import scala.language.implicitConversions

trait GrammarDocumentWriter {

  def identifier: BiGrammar = consume(Identifier)

  def number: BiGrammar = consume(NumberG)

  def integer = number ^^ ((s: Any) => Integer.parseInt(s.asInstanceOf[String]), (i: Any) => Some(i.toString))

  def failure: BiGrammar = BiFailure

  def produce(value: Any): BiGrammar = new ValueGrammar(value)

  def space: BiGrammar = print(new WhiteSpace(1, 1))

  def keyword(word: String): BiGrammar = new Keyword(word)

  implicit def consume(grammar: Grammar): BiGrammar = new FromGrammarWithToString(grammar)

  implicit def print(document: ResponsiveDocument): BiGrammar = new Print(document)

  implicit def print(document: Document): BiGrammar = new Print(document)

  implicit def stringToGrammar(value: String): BiGrammar =
    if (value.exists(c => Character.isLetterOrDigit(c))) //either exists or forall is a bit of an arbitrary choice. exists works better for syntax highlighting
      new Keyword(value)
    else new Delimiter(value)
}
