package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.particles.node.{GrammarKey, NodeField}
import core.responsiveDocument.ResponsiveDocument













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

  override def withChildren(newChildren: Seq[BiGrammar]) = new Choice(newChildren(0), newChildren(1))

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(left) || recursive(right)
}

class Sequence(var first: BiGrammar, var second: BiGrammar) extends BiGrammar with SequenceLike
{
  override def horizontal = true

  override def withChildren(newChildren: Seq[BiGrammar]) = new Sequence(newChildren(0), newChildren(1))
}

case class MapGrammar(var inner: BiGrammar,
                      construct: Any => Any,
                      deconstruct: Any => Option[Any],
                      showMap: Boolean = false) extends BiGrammar
{
  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = new MapGrammar(newChildren.head, construct, deconstruct, showMap)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
} //TODO deze nog wat meer typed maken met WithState

class Labelled(val name: GrammarKey, var inner: BiGrammar = BiFailure()) extends BiGrammar {

  def addOption(addition: BiGrammar) {
    inner = inner | addition
  }

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = new Labelled(name, newChildren.head)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}

trait Layout {
  def horizontal: Boolean
}

class TopBottom(var first: BiGrammar, var second: BiGrammar) extends BiGrammar with SequenceLike {
  override lazy val height: Int = first.height + second.height

  override def horizontal = false

  override def withChildren(newChildren: Seq[BiGrammar]) = new TopBottom(newChildren(0), newChildren(1))
}

/**
  * Prints a value, but parses nothing.
  */
case class Print(document: ResponsiveDocument) extends BiGrammarWithoutChildren {
  override def containsParser(recursive: BiGrammar => Boolean): Boolean = false
}

/**
  * Does not consume or produce any syntax, but simply produces or consumes a value.
  */
case class ValueGrammar(value: Any) extends BiGrammarWithoutChildren {
  override def containsParser(recursive: BiGrammar => Boolean): Boolean = false
}

case class As(var inner: BiGrammar, key: NodeField) extends BiGrammar
{
  override def children: Seq[BiGrammar] = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = As(newChildren.head, key)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}

case class BiFailure(message: String = "") extends BiGrammarWithoutChildren {
  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
