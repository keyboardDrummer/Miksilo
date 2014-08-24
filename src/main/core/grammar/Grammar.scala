package core.grammar

import scala.util.matching.Regex

trait GrammarWriter {

  def identifier = Identifier

  def number = NumberG

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

  override def toString = ToDocument.toDocument(ToDocument.transform(this)).renderString

  def <~(right: Grammar) = new IgnoreRight(this, right)

  def manySeparated(separator: Grammar): Grammar = someSeparated(separator) | new Produce(Seq.empty[Any])

  def |(other: Grammar) = new Choice(this, other)

  def someSeparated(separator: Grammar): Grammar = this ~ ((separator ~> this) *) ^^ {
    case first ~ rest => Seq(first) ++ rest.asInstanceOf[Seq[Any]]
  }

  def ~(other: Grammar) = new Sequence(this, other)

  def ~>(right: Grammar) = new IgnoreLeft(this, right)

  def * = new Many(this)

  def ^^(map: (Any) => Any): Grammar = new MapGrammar(this, map)

  def fold[T](empty: T, function: (Grammar => T, Grammar) => T) = {
    var closed = Set.empty[Grammar]

    def helper(grammar: Grammar): T = {
      if (closed.contains(grammar))
        return empty

      closed += grammar

      function((g: Grammar) => helper(g), grammar)
    }

    helper(this)
  }

}

case class RegexG(regex: Regex) extends Grammar

case class Many(var inner: Grammar) extends Grammar

case class IgnoreLeft(first: Grammar, second: Grammar) extends Grammar
{
  override def simplify = new MapGrammar(new Sequence(first, second), { case ~(l, r) => r})
}

case class IgnoreRight(first: Grammar, second: Grammar) extends Grammar
{
  override def simplify = new MapGrammar(new Sequence(first, second), { case ~(l, r) => l})
}

case class Choice(left: Grammar, right: Grammar) extends Grammar

case class Sequence(var first: Grammar, var second: Grammar) extends Grammar

case class Produce(result: Any) extends Grammar

case class Delimiter(value: String) extends Grammar {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")
}

case class Keyword(value: String) extends Grammar {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")
}

class MapGrammar(val inner: Grammar, val map: Any => Any) extends Grammar

class Labelled(val name: AnyRef, var inner: Grammar = null) extends Grammar {

  def orToInner(addition: Grammar) {
    inner = inner | addition
  }
}

object FailureG extends Grammar {
}

object NumberG extends Grammar {
}

object Identifier extends Grammar {
}