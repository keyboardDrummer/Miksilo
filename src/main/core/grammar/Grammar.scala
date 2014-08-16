package core.grammar

import scala.util.matching.Regex


trait GrammarWriter {

  def identifier = Identifier

  def number = NumberG

  def success = SuccessG

  def failure = FailureG

  def produce(value: Any) = new Produce(value)

  def keyword(word: String) = new Keyword(word)

  implicit def stringToGrammar(value: String): Grammar =
    if (value.forall(c => Character.isLetterOrDigit(c)))
      new Keyword(value)
    else new Delimiter(value)
}


trait Grammar extends GrammarWriter {

  def simplify: Grammar = this

  def <~(right: Grammar) = new IgnoreRight(this, right)

  def manySeparated(separator: Grammar): Grammar = someSeparated(separator) | new Produce(Seq.empty[Any])

  def |(other: Grammar) = new Choice(this, other)

  def someSeparated(separator: Grammar): Grammar = this ~ ((separator ~> this) *) ^^ {
    case first ~ rest => Seq(first) ++ rest.asInstanceOf[Seq[Any]]
  }

  def ~(other: Grammar) = new Sequence(this, other)

  def ~>(right: Grammar) = new IgnoreLeft(this, right)

  def * = new Many(this)

  def ^^(f: (Any) => Any): Grammar = new MapGrammar(this, f, s => s)
}

class RegexG(val regex: Regex) extends Grammar

class Many(var inner: Grammar) extends Grammar {
  override def toString: String = s"$inner*"
}

class IgnoreLeft(first: Grammar, second2: Grammar) extends Grammar {
  override def simplify = new MapGrammar(new Sequence(first, second2), { case ~(l, r) => r}, s => s)

  override def toString: String = s"$first ~> $second2"
}

class IgnoreRight(first: Grammar, second: Grammar) extends Grammar {
  override def simplify = new MapGrammar(new Sequence(first, second), { case ~(l, r) => l}, s => s)

  override def toString: String = s"$first <~ $second"
}

class Sequence(var first: Grammar, var second: Grammar) extends Grammar {
  override def toString: String = s"$first ~ $second"
}

class Produce(var result: Any) extends Grammar {

}

class Delimiter(var value: String) extends Grammar {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def toString: String = value
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

  def orToInner(addition: Grammar) {
    inner = inner | addition
  }
}