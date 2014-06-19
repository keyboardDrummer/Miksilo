package grammar

import scala.util.parsing.combinator.Parsers


trait GrammarWriter {


  def identifier = Identifier

  def number = Number

  def success = Success

  def failure = FailureG

  def produce(value: Any) = new Produce(value)

  def keyword(word: String) = new Keyword(word)

  implicit def stringToGrammar(value: String): Grammar = keyword(value)

}

case class seqr[+a, +b](_1: a, _2: b) extends scala.AnyRef with scala.Product with scala.Serializable {
}

trait Grammar extends Parsers {

  def mustConsume: Boolean

  def ~(other: Grammar) = new Sequence(this, other)

  def ~>(right: Grammar) = (this ~ right) ^^ {
    case seqr(l, r) => r
  }

  def <~(right: Grammar) = (this ~ right) ^^ { case seqr(l,r) => l }


  def |(other: Grammar) = new Choice(this, other)

  def * = new Many(this)

  def named(name: String) = new Labelled(name, this)

  def manySeparated(seperator: Grammar): Grammar = someSeparated(seperator) | new Produce(Seq.empty[Any])

  def ^^(f: (Any) => Any): Grammar = new MapGrammar(this, f, s => s)

  def someSeparated(seperator: Grammar): Grammar = this ~ ((seperator ~> this)*) ^^ {
    case first seqr rest => Seq(first) ++ rest.asInstanceOf[Seq[Any]]
  }

  def findGrammar(name: String): Grammar = {
    var closed = Set.empty[Grammar]
    def findGrammar(grammar: Grammar): Set[Grammar] = {
      if (closed.contains(grammar))
        return Set.empty

      this match {
        case labelled: Labelled => if (labelled.name == name)
          Set(labelled)
        else findGrammar(labelled.inner)
        case sequence: Sequence => findGrammar(sequence.first) ++ findGrammar(sequence.second)
        case choice: Choice => findGrammar(choice.left) ++ findGrammar(choice.right)
      }
    }
    findGrammar(this).head
  }
}

class Many(var inner: Grammar) extends Grammar {
  if (!inner.mustConsume)
    throw new RuntimeException("cannot build non-terminating grammars")

  override def mustConsume: Boolean = false
}

class Sequence(var first: Grammar, var second: Grammar) extends Grammar {

  override def mustConsume: Boolean = first.mustConsume || second.mustConsume
}

class Produce(var result: Any) extends Grammar {

  override def mustConsume: Boolean = false
}

class Keyword(var value: String) extends Grammar {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def mustConsume: Boolean = true
}

class Lazy(inner: => Grammar) extends Grammar {
  override def mustConsume: Boolean = inner.mustConsume
}

class Choice(var left: Grammar, var right: Grammar) extends Grammar {

  override def mustConsume: Boolean = left.mustConsume && right.mustConsume
}

class MapGrammar(val inner: Grammar, val forward: Any => Any, val backward: Any => Any) extends Grammar {
  override def mustConsume: Boolean = inner.mustConsume
}

class Labelled(val name: String, val inner: Grammar) extends Grammar {
  override def mustConsume: Boolean = inner.mustConsume
}