package core.grammarDocument

import core.grammar._
import core.responsiveDocument.ResponsiveDocument

trait GrammarDocumentWriter {

  def identifier: GrammarDocument = consume(Identifier)

  def number: GrammarDocument = consume(NumberG)

  def failure: GrammarDocument = FailureG

  def produce(value: Any): GrammarDocument = new Produce(value)

  def space: GrammarDocument = new WhiteSpace(1, 1)

  def keyword(word: String): GrammarDocument = new Keyword(word)

  implicit def consume(grammar: Grammar) = new Consume(grammar)

  implicit def stringToGrammar(value: String): GrammarDocument =
    if (value.forall(c => Character.isLetterOrDigit(c)))
      new Keyword(value)
    else new Delimiter(value)
}


trait GrammarDocument extends GrammarDocumentWriter {

  override def toString = PrintGrammar.toDocument(GrammarDocumentToGrammar.toGrammar(this)).renderString(false)

  def simplify: GrammarDocument = this

  def <~(right: GrammarDocument) = new IgnoreRight(this, right)

  def <~~(right: GrammarDocument) = this <~ (space ~ right)

  def manySeparated(separator: GrammarDocument): GrammarDocument = someSeparated(separator) | new Produce(Seq.empty[Any])

  def |(other: GrammarDocument) = new Choice(this, other)

  def ~~(right: GrammarDocument): GrammarDocument = {
    (this <~ space) ~ right
  }

  def someSeparated(separator: GrammarDocument): GrammarDocument = this ~ ((separator ~> this) *) ^^
    ( {
      case first ~ rest => Seq(first) ++ rest.asInstanceOf[Seq[Any]]
    }, {
      case seq: Seq[Any] => if (seq.nonEmpty) Some(core.grammar.~(seq.head, seq.tail)) else None
    })

  def ~(other: GrammarDocument) = new Sequence(this, other)

  def ~>(right: GrammarDocument) = new IgnoreLeft(this, right)

  def ~~>(right: GrammarDocument) = (this ~ space) ~> right

  def * = new Many(this)

  def %(bottom: GrammarDocument) = new TopBottom(this, bottom)

  def %>(bottom: GrammarDocument) = new TopBottom(this, bottom).ignoreLeft

  def %<(bottom: GrammarDocument) = new TopBottom(this, bottom).ignoreRight

  def ^^(map: (Any => Any, Any => Option[Any])): GrammarDocument = new MapGrammar(this, map._1, map._2)

  def indent(width: Int) = new WhiteSpace(width,0) ~> this

}

trait SequenceLike extends GrammarDocument
{
  def ignoreLeft: MapGrammar = {
    new MapGrammar(this, { case ~(l, r) => r}, r => Some(core.grammar.~(MissingValue, r)))
  }

  def ignoreRight: MapGrammar = {
    new MapGrammar(this, { case ~(l, r) => l}, l => Some(core.grammar.~(l, MissingValue)))
  }
}

case class Delimiter(value: String) extends GrammarDocument

case class Keyword(value: String) extends GrammarDocument

case class Consume(grammar: Grammar) extends GrammarDocument

case class Many(var inner: GrammarDocument) extends GrammarDocument

object MissingValue

case class IgnoreLeft(first: GrammarDocument, second: GrammarDocument) extends GrammarDocument {
  override def simplify = new Sequence(first, second).ignoreLeft
}

case class IgnoreRight(first: GrammarDocument, second: GrammarDocument) extends GrammarDocument {
  override def simplify = new Sequence(first, second).ignoreRight
}

case class Choice(left: GrammarDocument, right: GrammarDocument) extends GrammarDocument

case class Sequence(first: GrammarDocument, second: GrammarDocument) extends GrammarDocument with SequenceLike

case class MapGrammar(inner: GrammarDocument, construct: Any => Any, deconstruct: Any => Option[Any]) extends GrammarDocument

class Labelled(val name: AnyRef, var inner: GrammarDocument = FailureG) extends GrammarDocument {

  def orToInner(addition: GrammarDocument) {
    inner = inner | addition
  }
}

case class TopBottom(top: GrammarDocument, bottom: GrammarDocument) extends GrammarDocument with SequenceLike

case class WhiteSpace(width: Int, height: Int) extends GrammarDocument

case class Print(document: ResponsiveDocument) extends GrammarDocument

case class Produce(result: Any) extends GrammarDocument

object FailureG extends GrammarDocument