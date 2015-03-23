package core.bigrammar

import core.document.{BlankLine, Document, WhiteSpace}
import core.grammar.{Grammar, Identifier, NumberG, PrintGrammar, ~}
import core.particles.node.Node
import core.responsiveDocument.ResponsiveDocument

trait GrammarDocumentWriter {

  def identifier: BiGrammar = consume(Identifier)

  def number: BiGrammar = consume(NumberG)

  def integer = number ^^ ((s: Any) => Integer.parseInt(s.asInstanceOf[String]), (i: Any) => Some(i.toString))

  def failure: BiGrammar = BiFailure

  def produce(value: Any): BiGrammar = new Produce(value)

  def space: BiGrammar = print(new WhiteSpace(1, 1))

  def keyword(word: String): BiGrammar = new Keyword(word)

  implicit def consume(grammar: Grammar): BiGrammar = new Consume(grammar)

  implicit def print(document: ResponsiveDocument): BiGrammar = new Print(document)

  implicit def print(document: Document): BiGrammar = new Print(document)

  implicit def stringToGrammar(value: String): BiGrammar =
    if (value.forall(c => Character.isLetterOrDigit(c)))
      new Keyword(value)
    else new Delimiter(value)
}


trait BiGrammar extends GrammarDocumentWriter {

  override def toString = PrintGrammar.toDocument(BiGrammarToGrammar.toGrammar(this)).renderString(false)

  lazy val height = 1

  def <~(right: BiGrammar) = new Sequence(this, right).ignoreRight

  def <~~(right: BiGrammar) = this <~ (space ~ right)

  def manySeparated(separator: BiGrammar): BiGrammar = someSeparated(separator) | new Produce(Seq.empty[Any])

  def |(other: BiGrammar) = new Choice(this, other)

  def ~~(right: BiGrammar): BiGrammar = {
    (this <~ space) ~ right
  }

  def someSeparatedVertical(separator: BiGrammar): BiGrammar =
    this % new ManyVertical(separator %> this) ^^ separatedMap

  def manyVertical = new ManyVertical(this)

  def manySeparatedVertical(separator: BiGrammar): BiGrammar = someSeparatedVertical(separator) | new Produce(Seq.empty[Node])

  def someSeparated(separator: BiGrammar): BiGrammar = this ~ ((separator ~> this) *) ^^ separatedMap

  private def separatedMap: ((Any) => Seq[Any], (Any) => Option[~[Any, Seq[Any]]]) = {
    ( {
      case first ~ rest => Seq(first) ++ rest.asInstanceOf[Seq[Any]]
    }, {
      case seq: Seq[Any] => if (seq.nonEmpty) Some(core.grammar.~(seq.head, seq.tail)) else None
    })
  }
  
  def seqToSet: BiGrammar = new MapGrammar(this, seq => seq.asInstanceOf[Seq[Any]].toSet, set => Some(set.asInstanceOf[Set[Any]].toSeq))

  def inParenthesis = ("(": BiGrammar) ~> this <~ ")"

  def ~(other: BiGrammar) = new Sequence(this, other)

  def ~>(right: BiGrammar): BiGrammar = new Sequence(this, right).ignoreLeft

  def ~~>(right: BiGrammar) = (this ~ space) ~> right

  def * = new ManyHorizontal(this)

  def %(bottom: BiGrammar) = new TopBottom(this, bottom)

  def %%(bottom: BiGrammar): BiGrammar = {
    (this %< BlankLine) % bottom
  }

  def %>(bottom: BiGrammar) = new TopBottom(this, bottom).ignoreLeft

  def %<(bottom: BiGrammar) = new TopBottom(this, bottom).ignoreRight

  def ^^(map: (Any => Any, Any => Option[Any])): BiGrammar = new MapGrammar(this, map._1, map._2)

  def indent(width: Int = 2) = new WhiteSpace(width, 0) ~> this

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
      case _ => handleGrammar(grammar, helper)
    }
    helper(grammar)
  }
}

trait SequenceLike extends BiGrammar {
  def first: BiGrammar
  def second: BiGrammar
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
{
  override def toString = "_"
}

case class Choice(left: BiGrammar, right: BiGrammar) extends BiGrammar

case class Sequence(first: BiGrammar, second: BiGrammar) extends BiGrammar with SequenceLike

case class MapGrammar(inner: BiGrammar, construct: Any => Any, deconstruct: Any => Option[Any]) extends BiGrammar

class Labelled(val name: AnyRef, var inner: BiGrammar = BiFailure) extends BiGrammar {

  def addOption(addition: BiGrammar) {
    inner = inner | addition
  }
}

case class TopBottom(first: BiGrammar, second: BiGrammar) extends BiGrammar with SequenceLike {
  override lazy val height = first.height + second.height
}

case class Print(document: ResponsiveDocument) extends BiGrammar

case class Produce(result: Any) extends BiGrammar

object BiFailure extends BiGrammar