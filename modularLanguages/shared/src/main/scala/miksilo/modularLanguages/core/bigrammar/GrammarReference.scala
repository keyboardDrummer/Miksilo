package miksilo.modularLanguages.core.bigrammar

import miksilo.modularLanguages.core.bigrammar.grammars.{BiChoice, ValueGrammar}
import miksilo.modularLanguages.core.bigrammar.printer.UndefinedDestructuringValue
import util.Property

import scala.util.hashing.Hashing

class GrammarReference(val previous: GrammarPath, val property: Property[BiGrammar, BiGrammar]) extends GrammarPath
{
  def parent: BiGrammar = previous.value
  private var cachedValue: BiGrammar = _
  private var cachedHashCode: Option[Int] = None
  private var cachedAncestorGrammars: Set[BiGrammar] = _

  def ancestorGrammars: Set[BiGrammar] = {
    if (cachedAncestorGrammars == null)
      cachedAncestorGrammars = previous.ancestorGrammars + parent
    cachedAncestorGrammars
  }

  def value: BiGrammar = {
    if (cachedValue == null)
      cachedValue = property.get(parent).asInstanceOf[BiGrammar]
    cachedValue
  }

  def set(newValue: BiGrammar): Unit = {
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
    val choiceParent = parent.asInstanceOf[BiChoice]
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
