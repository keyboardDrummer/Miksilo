package core.bigrammar

import core.particles.node.{Key, NodeField}
import util.{ExtendedType, GraphBasics, Property}

import scala.collection.mutable
import scala.util.hashing.Hashing

object GrammarPath {
  val cache: mutable.Map[Class[_], List[Property[BiGrammar, AnyRef]]] = mutable.Map.empty

  def getBiGrammarProperties(clazz: Class[_]): List[Property[BiGrammar, AnyRef]] = {
    cache.getOrElseUpdate(clazz, new ExtendedType(clazz).properties.
      filter(property => classOf[BiGrammar].isAssignableFrom(property._type)).
      map(p => p.asInstanceOf[Property[BiGrammar, AnyRef]]).toList)
  }
}

trait GrammarPath {
  def value: BiGrammar
  def newChildren: List[GrammarReference] = children.filter(c => !ancestorGrammars.contains(c.value))

  def children: List[GrammarReference] = {
    val properties = GrammarPath.getBiGrammarProperties(value.getClass)
    properties.map(property => new GrammarReference(this, property))
  }

  def ancestorGrammars: Set[BiGrammar]
  def ancestors: Seq[GrammarPath]
  def findGrammar(grammar: BiGrammar): Option[GrammarPath] = find(p => p.value == grammar)

  def findAs(field: NodeField): GrammarReference = {
    find(p => p.value match { case as:As => as.key == field; case _ => false}).get.asInstanceOf[GrammarReference]
  }

  def findLabelled(label: Key): GrammarReference = {
    find(p => p.value match { case as:Labelled => as.name == label; case _ => false}).get.asInstanceOf[GrammarReference]
  }

  def find(predicate: GrammarPath => Boolean): Option[GrammarPath] = {
    var result: Option[GrammarPath] = None
    GraphBasics.traverseBreadth[GrammarPath](Seq(this),
      path => path.children.filter(c => !path.ancestorGrammars.contains(c.value)),
      path =>
        if (predicate(path)) {
          result = Some(path)
          false
        }
        else {
          true
        }
    )
    result
  }

  def descendants: Seq[GrammarReference] = selfAndDescendants.drop(1).collect { case x:GrammarReference => x }
  def selfAndDescendants: Seq[GrammarPath] = GraphBasics.traverseBreadth[GrammarPath](Seq(this),
    path => path.children.filter(c => !path.ancestorGrammars.contains(c.value)))
}

class RootGrammar(val value: BiGrammar) extends GrammarPath
{
  override def ancestorGrammars = Set(value)

  override def ancestors: Seq[GrammarPath] = Seq.empty

  override def hashCode(): Int = value.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case other: RootGrammar => value.equals(other.value)
    case _ => false
  }
}

class GrammarReference(val previous: GrammarPath, val property: Property[BiGrammar, AnyRef]) extends GrammarPath
{
  def parent: BiGrammar = previous.value
  private var cachedValue: BiGrammar = _
  private var cachedHashCode: Option[Int] = None
  private var cachedAncestorGrammars: Set[BiGrammar] = _

  def ancestorGrammars: Set[BiGrammar] = {
    if (cachedAncestorGrammars == null)
      cachedAncestorGrammars = previous.ancestorGrammars + value
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
    val choiceParent = parent.asInstanceOf[Choice]
    val me = value
    val sibling = Set(choiceParent.left,choiceParent.right).filter(grammar => grammar != me).head
    previous.asInstanceOf[GrammarReference].set(sibling)
  }

  def removeMeFromSequence(): Unit = {
    set(ValueGrammar(UndefinedDestructuringValue)) //TODO add transformation to remove ValueGrammar(Unit)
  }

  override def toString = s"$value <INSIDE> $parent"

  override def ancestors: Seq[GrammarPath] = Seq(previous) ++ previous.ancestors
}
