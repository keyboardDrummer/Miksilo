package core.bigrammar

import core.document.{BlankLine, Document, WhiteSpace}
import core.grammar.{Identifier, NumberG, ~}
import core.particles.node.Node
import core.responsiveDocument.ResponsiveDocument

import scala.language.implicitConversions

trait BiGrammarSequenceWriter extends BiGrammarWriter {

  implicit def stringAsGrammar(value: String) = new GrammarWithSequence(value)
  implicit class GrammarWithSequence(grammar: BiGrammar) {
    def indent(width: Int = 2) = GrammarWithSequence(WhiteSpace(width, 0)) ~> grammar

    def ~<(right: BiGrammar) = (this ~ right).ignoreRight

    def ~~<(right: BiGrammar) = this ~< (space ~ right)

    def manySeparated(separator: BiGrammar): BiGrammar = someSeparated(separator) | ValueGrammar(Seq.empty[Any])

    def ~~(right: BiGrammar): BiGrammar = {
      (this ~< space) ~ right
    }

    def someSeparatedVertical(separator: BiGrammar): BiGrammar =
      someMap(this % (separator %> grammar).manyVertical)

    def manyVertical = new ManyVertical(WithWhiteSpace(grammar))

    def manySeparatedVertical(separator: BiGrammar): BiGrammar = someSeparatedVertical(separator) | ValueGrammar(Seq.empty[Node])

    def some: BiGrammar = someMap(grammar ~ (grammar*))
    def someSeparated(separator: BiGrammar): BiGrammar = someMap(this ~ ((separator ~> grammar) *))

    private def someMap(grammar: BiGrammar): BiGrammar = {
      grammar ^^
        ( {
          case first ~ rest => Seq(first) ++ rest.asInstanceOf[Seq[Any]]
        }, {
          case seq: Seq[Any] => if (seq.nonEmpty) Some(core.grammar.~(seq.head, seq.tail)) else None
        })
    }
    def inParenthesis = ("(": BiGrammar) ~> grammar ~< ")"

    def ~(other: BiGrammar) = new Sequence(grammar, WithWhiteSpace(other))

    def ~>(right: BiGrammar): BiGrammar = (this ~ right).ignoreLeft

    def ~~>(right: BiGrammar) = (this ~ space) ~> right

    def * = new ManyHorizontal(WithWhiteSpace(grammar))
    def many = this*

    def %(bottom: BiGrammar) = new TopBottom(grammar, WithWhiteSpace(bottom))

    def %%(bottom: BiGrammar): BiGrammar = {
      (this %< BlankLine) % bottom
    }

    def %>(bottom: BiGrammar) = (this % bottom).ignoreLeft

    def %<(bottom: BiGrammar) = (this % bottom).ignoreRight
  }
}

trait BiGrammarWriter {

  def identifier: BiGrammar = new FromStringGrammar(Identifier)

  def number: BiGrammar = new FromGrammarWithToString(NumberG)

  def integer = number ^^ (
    (s: Any) => Integer.parseInt(s.asInstanceOf[String]),
    (i: Any) => Some(i).filter(i => i.isInstanceOf[Int]).map(i => i.toString)
  )

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
