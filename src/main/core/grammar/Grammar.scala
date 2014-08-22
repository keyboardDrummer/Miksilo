package core.grammar

import core.responsiveDocument.ResponsiveDocument

import scala.collection.immutable.Stream.Cons
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

  def toDocument : ResponsiveDocument
  override def toString = toDocument.renderString

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

  def ^^(map: (Any) => Any): Grammar = new MapGrammar(this, map)

  def traverse(): Stream[Grammar] = {
    var closed = Set.empty[Grammar]

    def helper(grammar: Grammar): Stream[Grammar] = {
      if (closed.contains(grammar))
        return Stream.empty
      
      closed += grammar
      
      new Cons(grammar, grammar.simplify match {
        case choice: Choice => helper(choice.left) ++ helper(choice.right)
        case sequence: Sequence => helper(sequence.first) ++ helper(sequence.second)
        case many: Many => helper(many.inner)
        case labelled: Labelled => helper(labelled.inner)
        case map: MapGrammar => helper(map.inner)
        case x => Stream.empty
      })
    }

    helper(this)
  }

}

class RegexG(val regex: Regex) extends Grammar {
  override def toDocument: ResponsiveDocument = s"Regex($regex)"
}

class Many(var inner: Grammar) extends Grammar {
  override def toDocument: ResponsiveDocument = ResponsiveDocument.text("(") ~ inner.toDocument ~ ")" ~ "*"
}

class IgnoreLeft(first: Grammar, second: Grammar) extends Grammar {
  override def simplify = new MapGrammar(new Sequence(first, second), { case ~(l, r) => r})
  override def toDocument: ResponsiveDocument = first.toDocument | second.toDocument
}

class IgnoreRight(first: Grammar, second: Grammar) extends Grammar {
  override def simplify = new MapGrammar(new Sequence(first, second), { case ~(l, r) => l})
  override def toDocument: ResponsiveDocument = first.toDocument | second.toDocument
}

class Sequence(var first: Grammar, var second: Grammar) extends Grammar {
  override def toDocument: ResponsiveDocument = first.toDocument | second.toDocument
}

class Produce(var result: Any) extends Grammar {
  override def toDocument: ResponsiveDocument = ""
}

class Delimiter(var value: String) extends Grammar {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def toDocument: ResponsiveDocument = value
}

class Keyword(var value: String) extends Grammar {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def toDocument: ResponsiveDocument = value
}

class Choice(var left: Grammar, var right: Grammar) extends Grammar {
  override def toDocument: ResponsiveDocument = ResponsiveDocument.text("(") ~ left.toDocument | "^" | right.toDocument ~ ")"
}

class MapGrammar(val inner: Grammar, val map: Any => Any) extends Grammar {
  override def toString: String = inner.toString
  override def toDocument: ResponsiveDocument = inner.toDocument // ResponsiveDocument.text("(") ~ inner.toDocument ~ ")"
}

class Labelled(val name: AnyRef, var inner: Grammar = null) extends Grammar {

  def orToInner(addition: Grammar) {
    inner = inner | addition
  }

  override def toDocument: ResponsiveDocument = name.getClass.getSimpleName
}

object FailureG extends Grammar {
  override def toDocument: ResponsiveDocument = "fail"
}

object SuccessG extends Grammar {
  override def toDocument: ResponsiveDocument = "success"
}

object NumberG extends Grammar {
  override def toDocument: ResponsiveDocument = "number"
}

object Identifier extends Grammar {
  override def toDocument: ResponsiveDocument = "identifier"
}