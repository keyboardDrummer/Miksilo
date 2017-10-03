package core.bigrammar

import core.document.{BlankLine, WhiteSpace}
import core.grammar.{Grammar, PrintGrammar, ~}
import core.particles.node.{Node, NodeField}
import core.responsiveDocument.ResponsiveDocument

/*
A grammar that maps to both a parser and a printer
 */
trait BiGrammar extends GrammarDocumentWriter {

  def getDescendantsAndSelf: Seq[BiGrammar] = ???

  override def toString = PrintGrammar.toDocument(BiGrammarToGrammar.toGrammar(this)).renderString(trim = false)

  lazy val height = 1

  def <~(right: BiGrammar) = new Sequence(this, right).ignoreRight

  def <~~(right: BiGrammar) = this <~ (space ~ right)

  def manySeparated(separator: BiGrammar): BiGrammar = someSeparated(separator) | ValueGrammar(Seq.empty[Any])

  def |(other: BiGrammar) = new Choice(this, other)

  def ~~(right: BiGrammar): BiGrammar = {
    (this <~ space) ~ right
  }

  def someSeparatedVertical(separator: BiGrammar): BiGrammar =
    this % new ManyVertical(separator %> this) ^^ someMap

  def manyVertical = new ManyVertical(this)

  def manySeparatedVertical(separator: BiGrammar): BiGrammar = someSeparatedVertical(separator) | ValueGrammar(Seq.empty[Node])

  def option: BiGrammar = this ^^ (x => Some(x), x => x.asInstanceOf[Option[Any]]) | produce(None)
  def some: BiGrammar = this ~ (this*) ^^ someMap
  def someSeparated(separator: BiGrammar): BiGrammar = this ~ ((separator ~> this) *) ^^ someMap
  def children: Seq[BiGrammar] = Seq.empty

  private def someMap: ((Any) => Seq[Any], (Any) => Option[~[Any, Seq[Any]]]) = {
    ( {
      case first ~ rest => Seq(first) ++ rest.asInstanceOf[Seq[Any]]
    }, {
      case seq: Seq[Any] => if (seq.nonEmpty) Some(core.grammar.~(seq.head, seq.tail)) else None
    })
  }

  def optionToSeq: BiGrammar = new MapGrammar(this,
    option => option.asInstanceOf[Option[Any]].fold(Seq.empty[Any])(x => Seq(x)), {
      case seq:Seq[Any] => Some(if (seq.isEmpty) None else Some(seq))
      case _ => None
    })
  def seqToSet: BiGrammar = new MapGrammar(this, seq => seq.asInstanceOf[Seq[Any]].toSet, set => Some(set.asInstanceOf[Set[Any]].toSeq))

  def inParenthesis = ("(": BiGrammar) ~> this <~ ")"

  def ~(other: BiGrammar) = new Sequence(this, other)

  def ~>(right: BiGrammar): BiGrammar = new Sequence(this, right).ignoreLeft

  def ~~>(right: BiGrammar) = (this ~ space) ~> right

  def * = new ManyHorizontal(this)
  def many = new ManyHorizontal(this)

  def %(bottom: BiGrammar) = new TopBottom(this, bottom)

  def %%(bottom: BiGrammar): BiGrammar = {
    (this %< BlankLine) % bottom
  }

  def %>(bottom: BiGrammar) = new TopBottom(this, bottom).ignoreLeft

  def %<(bottom: BiGrammar) = new TopBottom(this, bottom).ignoreRight

  def ^^(map: (Any => Any, Any => Option[Any])): BiGrammar = new MapGrammar(this, map._1, map._2)

  def indent(width: Int = 2) = WhiteSpace(width, 0) ~> this

  def deepClone: BiGrammar = new DeepCloneBiGrammar().observe(this)
}

trait SequenceLike extends BiGrammar {
  def first: BiGrammar
  def second: BiGrammar
  def ignoreLeft: MapGrammar = {
    new MapGrammar(this, { case ~(l, r) => {
      val x = this
      r
    }}, r => Some(core.grammar.~(UndefinedDestructuringValue, r)))
  }

  def ignoreRight: MapGrammar = {
    new MapGrammar(this, { case ~(l, r) => l}, l => Some(core.grammar.~(l, UndefinedDestructuringValue)))
  }
}

case class Delimiter(value: String) extends BiGrammar

case class Keyword(value: String, reserved: Boolean = true, verifyWhenPrinting: Boolean = false) extends BiGrammar

/**
  * Takes a grammar for parsing, and uses toString for printing.
  * so the result of the grammar is exactly what has been consumed.
  * @verifyWhenPrinting When printing, make sure the string to print can be consumed by the grammar.
  */
case class FromGrammarWithToString(grammar: Grammar, verifyWhenPrinting: Boolean = false) extends BiGrammar

abstract class Many(var inner: BiGrammar) extends BiGrammar
{
  override def children = Seq(inner)
}

class ManyVertical(inner: BiGrammar) extends Many(inner)

class ManyHorizontal(inner: BiGrammar) extends Many(inner)

/*
Used in destructuring when a value is required as a result but it's not in the object to be destructured.
*/
object UndefinedDestructuringValue //TODO looks a bit like ValueNotFound. Combine??
{
  override def toString = "_"
}

class Choice(var left: BiGrammar, var right: BiGrammar, val firstBeforeSecond: Boolean = false) extends BiGrammar
{
  override def children = Seq(left, right)
}

class Sequence(var first: BiGrammar, var second: BiGrammar) extends BiGrammar with SequenceLike
{
  override def children = Seq(first, second)
}

class MapGrammar(var inner: BiGrammar, val construct: Any => Any, val deconstruct: Any => Option[Any], val showMap: Boolean = false) extends BiGrammar
{
  override def children = Seq(inner)
} //TODO deze nog wat meer typed maken met WithState

class Labelled(val name: AnyRef, var inner: BiGrammar = BiFailure()) extends BiGrammar {

  def addOption(addition: BiGrammar) {
    inner = inner | addition
  }

  override def children = Seq(inner)
}

class TopBottom(var first: BiGrammar, var second: BiGrammar) extends BiGrammar with SequenceLike {
  override lazy val height: Int = first.height + second.height

  override def children = Seq(first, second)
}

/**
  * Prints a value, but parses nothing.
  */
case class Print(document: ResponsiveDocument) extends BiGrammar

/**
  * Does not consume or produce any syntax, but simply produces or consumes a value.
  */
case class ValueGrammar(value: Any) extends BiGrammar

case class As(var inner: BiGrammar, key: NodeField) extends BiGrammar

case class BiFailure(message: String = "") extends BiGrammar