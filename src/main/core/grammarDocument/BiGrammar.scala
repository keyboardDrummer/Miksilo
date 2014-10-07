package core.grammarDocument

import core.document.{BlankLine, Document, WhiteSpace}
import core.grammar.{Grammar, Identifier, NumberG, PrintGrammar, ~}
import core.responsiveDocument.ResponsiveDocument
import core.transformation.MetaObject

trait GrammarDocumentWriter {

  def identifier: BiGrammar = consume(Identifier)

  def number: BiGrammar = consume(NumberG)

  def failure: BiGrammar = BiFailure

  def produce(value: Any): BiGrammar = new Produce(value)

  def space: BiGrammar = print(new WhiteSpace(1, 1))

  def keyword(word: String): BiGrammar = new Keyword(word)

  implicit def consume(grammar: Grammar) = new Consume(grammar)

  implicit def print(document: ResponsiveDocument) = new Print(document)

  implicit def print(document: Document) = new Print(document)

  implicit def stringToGrammar(value: String): BiGrammar =
    if (value.forall(c => Character.isLetterOrDigit(c)))
      new Keyword(value)
    else new Delimiter(value)
}


trait BiGrammar extends GrammarDocumentWriter {

  override def toString = PrintGrammar.toDocument(BiGrammarToGrammar.toGrammar(this)).renderString(false)

  def simplifySelf: BiGrammar = this
  def simplify = SimplifyBiGrammar.observe(this)

  lazy val height = 1

  def <~(right: BiGrammar) = new IgnoreRight(this, right)

  def <~~(right: BiGrammar) = this <~ (space ~ right)

  def manySeparated(separator: BiGrammar): BiGrammar = someSeparated(separator) | new Produce(Seq.empty[Any])

  def |(other: BiGrammar) = new Choice(this, other)

  def ~~(right: BiGrammar): BiGrammar = {
    (this <~ space) ~ right
  }

  def someSeparatedVertical(separator: BiGrammar): BiGrammar =
    this % new ManyVertical(separator %> this) ^^ separatedMap

  def manySeparatedVertical(separator: BiGrammar): BiGrammar = someSeparatedVertical(separator) | new Produce(Seq.empty[MetaObject])

  def someSeparated(separator: BiGrammar): BiGrammar = this ~ ((separator ~> this) *) ^^ separatedMap

  private def separatedMap: ((Any) => Seq[Any], (Any) => Option[~[Any, Seq[Any]]]) = {
    ( {
      case first ~ rest => Seq(first) ++ rest.asInstanceOf[Seq[Any]]
    }, {
      case seq: Seq[Any] => if (seq.nonEmpty) Some(core.grammar.~(seq.head, seq.tail)) else None
    })
  }

  def inParenthesis = ("(": BiGrammar) ~> this <~ ")"

  def ~(other: BiGrammar) = new Sequence(this, other)

  def ~>(right: BiGrammar) = new IgnoreLeft(this, right)

  def ~~>(right: BiGrammar) = (this ~ space) ~> right

  def * = new ManyHorizontal(this)

  def %(bottom: BiGrammar) = new TopBottom(this, bottom)

  def %%(bottom: BiGrammar): BiGrammar = {
    (this %< BlankLine) % bottom
  }

  def %>(bottom: BiGrammar) = new TopBottom(this, bottom).ignoreLeft

  def %<(bottom: BiGrammar) = new TopBottom(this, bottom).ignoreRight

  def ^^(map: (Any => Any, Any => Option[Any])): BiGrammar = new MapGrammar(this, map._1, map._2)

  def indent(width: Int) = new WhiteSpace(width, 0) ~> this

  def deepClone: BiGrammar = new DeepCloneBiGrammar().observe(this)
}

class DeepCloneBiGrammar extends BiGrammarObserver[BiGrammar] {

  override def labelledEnter(name: AnyRef): BiGrammar = new Labelled(name)

  override def labelledLeave(inner: BiGrammar, partial: BiGrammar): Unit = partial.asInstanceOf[Labelled].inner = inner

  override def handleGrammar(self: BiGrammar, helper: (BiGrammar) => BiGrammar): BiGrammar = self match {
    case Choice(first, second) => Choice(helper(first), helper(second))
    case many: ManyVertical => new ManyVertical(helper(many.inner))
    case many: ManyHorizontal => new ManyHorizontal(helper(many.inner))
    case MapGrammar(inner, construct, deconstruct) => MapGrammar(helper(inner), construct, deconstruct)
    case Sequence(first, second) => Sequence(helper(first), helper(second))
    case TopBottom(top, bottom) => TopBottom(helper(top), helper(bottom))
    case _ => self
  }
}

object SimplifyBiGrammar extends DeepCloneBiGrammar {
  override def handleGrammar(self: BiGrammar, helper: (BiGrammar) => BiGrammar): BiGrammar = super.handleGrammar(self.simplifySelf, helper)
}

trait BiGrammarObserver[Result] {

  def labelledEnter(name: AnyRef): Result

  def labelledLeave(inner: Result, partial: Result)

  def handleGrammar(self: BiGrammar, recursive: BiGrammar => Result): Result

  def observe(grammar: BiGrammar) = {
    var cache = Map.empty[Labelled, Result]
    def helper(grammar: BiGrammar): Result = grammar match {
      case labelled: Labelled =>
        cache.getOrElse(labelled, {
          val result = labelledEnter(labelled.name)
          cache += labelled -> result
          labelledLeave(helper(labelled.inner), result)
          result
        })
      case grammar => handleGrammar(grammar, helper)
    }
    helper(grammar)
  }
}

trait SequenceLike extends BiGrammar {
  def ignoreLeft: MapGrammar = {
    new MapGrammar(this, { case ~(l, r) => r}, r => Some(core.grammar.~(MissingValue, r)))
  }

  def ignoreRight: MapGrammar = {
    new MapGrammar(this, { case ~(l, r) => l}, l => Some(core.grammar.~(l, MissingValue)))
  }
}

case class Delimiter(value: String) extends BiGrammar

case class Keyword(value: String) extends BiGrammar

case class Consume(grammar: Grammar) extends BiGrammar

abstract case class Many(inner: BiGrammar) extends BiGrammar

class ManyVertical(inner: BiGrammar) extends Many(inner)

class ManyHorizontal(inner: BiGrammar) extends Many(inner)

object MissingValue

case class IgnoreLeft(first: BiGrammar, second: BiGrammar) extends BiGrammar {
  override def simplifySelf = new Sequence(first, second).ignoreLeft
}

case class IgnoreRight(first: BiGrammar, second: BiGrammar) extends BiGrammar {
  override def simplifySelf = new Sequence(first, second).ignoreRight
}

case class Choice(left: BiGrammar, right: BiGrammar) extends BiGrammar

case class Sequence(first: BiGrammar, second: BiGrammar) extends BiGrammar with SequenceLike

case class MapGrammar(inner: BiGrammar, construct: Any => Any, deconstruct: Any => Option[Any]) extends BiGrammar

class Labelled(val name: AnyRef, var inner: BiGrammar = BiFailure) extends BiGrammar {

  def addOption(addition: BiGrammar) {
    inner = inner | addition
  }
}

case class TopBottom(top: BiGrammar, bottom: BiGrammar) extends BiGrammar with SequenceLike {
  override lazy val height = top.height + bottom.height
}

case class Print(document: ResponsiveDocument) extends BiGrammar

case class Produce(result: Any) extends BiGrammar

case class RowRepeater(inners: Seq[BiGrammar], construct: Seq[Any] => Any, deconstruct: Any => Option[Seq[Any]]) extends BiGrammar
{
}

object BiFailure extends BiGrammar