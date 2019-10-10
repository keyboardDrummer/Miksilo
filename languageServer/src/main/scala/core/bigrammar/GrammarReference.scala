package core.bigrammar

import core.bigrammar.grammars.{BiChoice, ValueGrammar}
import core.bigrammar.printer.UndefinedDestructuringValue
import util.Property
import scala.language.existentials
import scala.util.hashing.Hashing

class GrammarReference(val previous: GrammarPath, val property: Property[BiGrammar[_], BiGrammar[_]]) extends GrammarPath
{
  def parent: BiGrammar[_] = previous.value
  private var cachedValue: BiGrammar[_] = _
  private var cachedHashCode: Option[Int] = None
  private var cachedAncestorGrammars: Set[BiGrammar[_]] = _

  def ancestorGrammars: Set[BiGrammar[_]] = {
    if (cachedAncestorGrammars == null)
      cachedAncestorGrammars = previous.ancestorGrammars + parent
    cachedAncestorGrammars
  }

  def value: BiGrammar[_] = {
    if (cachedValue == null)
      cachedValue = property.get(parent)
    cachedValue
  }

  def set(newValue: BiGrammar[_]): Unit = {
    property.set(parent, newValue)
    cachedValue = null
    cachedAncestorGrammars = null
    cachedHashCode = None
  }

  override def hashCode(): Int = {
    if (cachedHashCode.isEmpty)
      cachedHashCode = Some(Hashing.default.hash((previous.hashCode(), property.hashCode())))
    cachedHashCode.get
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: GrammarReference =>
      other.property.equals(property) && other.previous.equals(previous)
    case _ => false
  }

  def removeMeFromOption(): Unit = {
    val choiceParent = parent.asInstanceOf[BiChoice[_]]
    val me = value
    val sibling = Set(choiceParent.left,choiceParent.right).filter(grammar => grammar != me).head
    previous.asInstanceOf[GrammarReference].set(sibling)
  }

  def removeMe(): Unit = {
    set(ValueGrammar(UndefinedDestructuringValue)) //TODO add transformation to remove ValueGrammar(Unit)
  }

  override def toString = s"$value <INSIDE> $parent"

  override def ancestors: Seq[GrammarPath] = Seq(previous) ++ previous.ancestors
}
