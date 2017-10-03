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
  def get: BiGrammar
  lazy val children: Seq[GrammarReference] = {
    GrammarPath.getBiGrammarProperties(get.getClass).map(property => new GrammarReference(this, property))
  }

  def ancestorGrammars: Set[BiGrammar]
  def ancestors: Seq[GrammarPath]
  def findGrammar(grammar: BiGrammar): Option[GrammarPath] = find(p => p.get == grammar)

  def findAs(field: NodeField): GrammarReference = {
    find(p => p.get match { case as:As => as.key == field; case _ => false}).get.asInstanceOf[GrammarReference]
  }

  def findLabelled(label: Key): GrammarReference = {
    find(p => p.get match { case as:Labelled => as.name == label; case _ => false}).get.asInstanceOf[GrammarReference]
  }

  def find(predicate: GrammarPath => Boolean): Option[GrammarPath] = {
    var result: Option[GrammarPath] = None
    GraphBasics.traverseBreadth[GrammarPath](Seq(this),
      path => path.children.filter(c => !path.ancestorGrammars.contains(c.get)),
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
    path => path.children.filter(c => !path.ancestorGrammars.contains(c.get)))
}

class RootGrammar(val value: BiGrammar) extends GrammarPath
{
  override def get: BiGrammar = value


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
  val parent: BiGrammar = previous.get

  def set(value: BiGrammar): Unit = {
    property.set(parent, value)
  }

  override def hashCode(): Int = Hashing.default.hash((previous.hashCode(), property.hashCode()))

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: GrammarReference =>
      other.property.equals(property) && other.previous.equals(previous)
    case _ => false
  }

  lazy val get: BiGrammar = {
    property.get(parent).asInstanceOf[BiGrammar]
  }
  
  def removeMeFromOption(): Unit = {
    val choiceParent = parent.asInstanceOf[Choice]
    val me = get
    val sibling = Set(choiceParent.left,choiceParent.right).filter(grammar => grammar != me).head 
    previous.asInstanceOf[GrammarReference].set(sibling)
  }

  def removeMeFromSequence(): Unit = {
    val choiceParent = parent.asInstanceOf[SequenceLike]
    val me = get
    val sibling = Set(choiceParent.first,choiceParent.second).filter(grammar => grammar != me).head
    previous.asInstanceOf[GrammarReference].set(sibling)
  }

  override def toString = s"GrammarSelection($get)"

  override def ancestors: Seq[GrammarPath] = Seq(previous) ++ previous.ancestors

  lazy val ancestorGrammars: Set[BiGrammar] = previous.ancestorGrammars + get
}
