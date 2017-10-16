package core.bigrammar

import core.bigrammar.BiGrammarToGrammar.{StateM, WithMap}
import core.bigrammar.printer.TryState.{NodePrinter, State}
import core.bigrammar.printer.{BiGrammarToPrinter, TryState}
import core.document.{BlankLine, Empty, WhiteSpace}
import core.grammar.{Grammar, GrammarToParserConverter, PrintGrammar, ~}
import core.particles.node.{Node, NodeField}
import core.responsiveDocument.ResponsiveDocument

import scala.util.matching.Regex
import scala.util.{Success, Try}
import scala.util.parsing.input.CharArrayReader

/*
A grammar that maps to both a parser and a printer
 */
trait BiGrammar extends BiGrammarWriter {

  def getDescendantsAndSelf: Seq[BiGrammar] = ???

  override def toString = PrintGrammar.toDocument(BiGrammarToGrammar.toGrammar(this)).renderString(trim = false)

  lazy val height = 1

  def ~<(right: BiGrammar) = new Sequence(this, right).ignoreRight

  def ~~<(right: BiGrammar) = this ~< (space ~ right)

  def manySeparated(separator: BiGrammar): BiGrammar = someSeparated(separator) | ValueGrammar(Seq.empty[Any])

  def |(other: BiGrammar) = new Choice(this, other)

  def ~~(right: BiGrammar): BiGrammar = {
    (this ~< space) ~ right
  }

  def someSeparatedVertical(separator: BiGrammar): BiGrammar =
    someMap(this % new ManyVertical(separator %> this))

  def manyVertical = new ManyVertical(this)

  def manySeparatedVertical(separator: BiGrammar): BiGrammar = someSeparatedVertical(separator) | ValueGrammar(Seq.empty[Node])

  def option: BiGrammar = this ^^ (x => Some(x), x => x.asInstanceOf[Option[Any]]) | value(None)
  def some: BiGrammar = someMap(this ~ (this*))
  def someSeparated(separator: BiGrammar): BiGrammar = someMap(this ~ ((separator ~> this) *))
  def children: Seq[BiGrammar] = Seq.empty

  private def someMap(grammar: BiGrammar): BiGrammar = {
    grammar ^^
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

  def inParenthesis = ("(": BiGrammar) ~> this ~< ")"

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

  def ^^(afterParsing: Any => Any, beforePrinting: Any => Option[Any]): BiGrammar =
    new MapGrammar(this, afterParsing, beforePrinting)

  def indent(width: Int = 2) = WhiteSpace(width, 0) ~> this

  def deepClone: BiGrammar = new DeepCloneBiGrammar().observe(this)
}

object StringLiteral extends CustomGrammar {
  override def getGrammar = core.grammar.StringLiteral
  override def write(from: WithMapG[Any], state: State) = Try(state, "\"" + from.value + "\"")
}

trait SuperCustomGrammar extends BiGrammar {
  def createGrammar(map: BiGrammar => Grammar): Grammar
  def createPrinter(map: BiGrammar => NodePrinter): NodePrinter
}

trait CustomGrammar extends BiGrammar with NodePrinter {
  def getGrammar: Grammar
}

trait SequenceLike extends BiGrammar with Layout {
  def first: BiGrammar
  def first_=(value: BiGrammar): Unit

  def second: BiGrammar
  def second_=(value: BiGrammar): Unit

  override def children = Seq(first, second)

  def ignoreLeft: MapGrammar = {
    new MapGrammar(this,
      { case ~(l, r) => r },
      r => Some(core.grammar.~(UndefinedDestructuringValue, r)))
  }

  def ignoreRight: MapGrammar = {
    new MapGrammar(this, { case ~(l, r) => l}, l => Some(core.grammar.~(l, UndefinedDestructuringValue)))
  }
}

case class Delimiter(value: String) extends BiGrammar

case class Keyword(value: String, reserved: Boolean = true, verifyWhenPrinting: Boolean = false) extends BiGrammar

class FromGrammarWithToString(grammar: Grammar, verifyWhenPrinting: Boolean = true)
  extends FromStringGrammar(grammar, verifyWhenPrinting) {

  override def write(from: WithMap, state: State) =
    super.write(WithMapG(from.value.toString, from.map), state)
}

/**
  * Takes a grammar for parsing, and uses toString for printing.
  * so the result of the grammar is exactly what has been consumed.
  * @verifyWhenPrinting When printing, make sure the string to print can be consumed by the grammar.
  */
class FromStringGrammar(grammar: Grammar, verifyWhenPrinting: Boolean = false)
  extends CustomGrammar
{
  override def getGrammar = grammar

  lazy val parser = GrammarToParserConverter.convert(grammar)

  override def write(from: WithMap, state: State) = {
    from.value match {
      case string: String =>
        if (verifyWhenPrinting) {
          val parseResult = parser(new CharArrayReader(string.toCharArray))
          if (parseResult.successful && parseResult.get.equals(from.value))
            Success(state, string)
          else
            TryState.fail("FromGrammarWithToString could not parse string")
        }
        else
          Success(state, string)

      case _ => TryState.fail(s"FromStringGrammar expects a string value, and not a $from")
    }
  }
}

case class RegexG(regex: Regex) extends FromStringGrammar(core.grammar.RegexG(regex))

abstract class Many(var inner: BiGrammar) extends BiGrammar with Layout
{
  override def children = Seq(inner)
}

class ManyVertical(inner: BiGrammar) extends Many(inner) {
  override def horizontal = false
}

class ManyHorizontal(inner: BiGrammar) extends Many(inner) {
  override def horizontal = true
}

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

  override def horizontal = true
}

class MapGrammar(var inner: BiGrammar, val construct: Any => Any, val deconstruct: Any => Option[Any],
                 val showMap: Boolean = false) extends BiGrammar
{
  override def children = Seq(inner)
} //TODO deze nog wat meer typed maken met WithState

class Labelled(val name: AnyRef, var inner: BiGrammar = BiFailure()) extends BiGrammar {

  def addOption(addition: BiGrammar) {
    inner = inner | addition
  }

  override def children = Seq(inner)
}

trait Layout {
  def horizontal: Boolean
}

class TopBottom(var first: BiGrammar, var second: BiGrammar) extends BiGrammar with SequenceLike {
  override lazy val height: Int = first.height + second.height

  override def horizontal = false
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
{
  override def children: Seq[BiGrammar] = Seq(inner)
}

case class BiFailure(message: String = "") extends BiGrammar