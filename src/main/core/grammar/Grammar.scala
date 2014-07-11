package core.grammar

import scala.util.parsing.combinator.Parsers


trait GrammarWriter {


  def identifier = Identifier

  def number = NumberG

  def success = SuccessG

  def failure = FailureG

  def produce(value: Any) = new Produce(value)

  def keyword(word: String) = new Keyword(word)

  implicit def stringToGrammar(value: String): Grammar = keyword(value)

}

case class seqr[+a, +b](_1: a, _2: b) extends scala.AnyRef with scala.Product with scala.Serializable {
}

trait Grammar extends Parsers {

  def ~(other: Grammar) = new Sequence(this, other)

  def ~>(right: Grammar) = new IgnoreLeft(this, right)

  def <~(right: Grammar) = new IgnoreRight(this, right)


  def |(other: Grammar) = new Choice(this, other)

  def * = new Many(this)

  def manySeparated(separator: Grammar): Grammar = someSeparated(separator) | new Produce(Seq.empty[Any])

  def ^^(f: (Any) => Any): Grammar = new MapGrammar(this, f, s => s)

  def someSeparated(separator: Grammar): Grammar = this ~ ((separator ~> this) *) ^^ {
    case first seqr rest => Seq(first) ++ rest.asInstanceOf[Seq[Any]]
  }

  def findGrammar(name: AnyRef): Labelled = {
    var closed = Set.empty[Grammar]
    def findGrammar(grammar: Grammar): Set[Labelled] = {
      if (closed.contains(grammar))
        return Set.empty

      closed += grammar

      grammar match {
        case labelled: Labelled =>
          if (labelled.name.equals(name))
            Set(labelled)
          else findGrammar(labelled.inner)
        case sequence: Sequence => findGrammar(sequence.first) ++ findGrammar(sequence.second)
        case choice: Choice => findGrammar(choice.left) ++ findGrammar(choice.right)
        case map: MapGrammar => findGrammar(map.inner)
        case many: Many => findGrammar(many.inner)
        case _ => Set.empty
      }
    }
    findGrammar(this).head
  }
}

class Many(var inner: Grammar) extends Grammar {
  override def toString: String = s"$inner*"
}

class IgnoreLeft(first: Grammar, second: Grammar)
  extends MapGrammar(new Sequence(first, second), { case seqr(l, r) => r}, s => s) {
  override def toString: String = s"$first ~> $second"
}

class IgnoreRight(first: Grammar, second: Grammar)
  extends MapGrammar(new Sequence(first, second), { case seqr(l, r) => l}, s => s) {
  override def toString: String = s"$first <~ $second"
}


class Sequence(var first: Grammar, var second: Grammar) extends Grammar {
  override def toString: String = s"$first ~ $second"
}

class Produce(var result: Any) extends Grammar {

}

class Keyword(var value: String) extends Grammar {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def toString: String = value
}


class Choice(var left: Grammar, var right: Grammar) extends Grammar {
  override def toString: String = s"$left | $right"
}

class MapGrammar(val inner: Grammar, val forward: Any => Any, val backward: Any => Any) extends Grammar {
  override def toString: String = inner.toString
}

class Labelled(val name: AnyRef, var inner: Grammar = null) extends Grammar {
  override def toString: String = name.getClass.getSimpleName
}