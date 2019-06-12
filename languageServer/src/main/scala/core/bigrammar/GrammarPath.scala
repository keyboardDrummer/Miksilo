package core.bigrammar

import core.bigrammar.grammars._
import core.language.node.{Key, NodeField, NodeGrammar, NodeShape}
import util.GraphBasics.{Continue, Halt}
import util.{ExtendedType, GraphBasics, Property}

import scala.collection.concurrent._

object GrammarPath {
  val cache: TrieMap[Class[_], List[Property[BiGrammar, BiGrammar]]] = TrieMap.empty

  def getBiGrammarProperties(clazz: Class[_]): List[Property[BiGrammar, BiGrammar]] = {
    cache.getOrElseUpdate(clazz, new ExtendedType(clazz).properties.
      filter(property => classOf[BiGrammar].isAssignableFrom(property._type)).
      map(p => p.asInstanceOf[Property[BiGrammar, BiGrammar]]).toList)
  }
}

trait GrammarPath {
  def value: BiGrammar
  def newChildren: List[GrammarReference] = children.filter(c => !seenGrammars.contains(c.value))

  def children: List[GrammarReference] = {
    val properties = GrammarPath.getBiGrammarProperties(value.getClass)
    properties.map(property => new GrammarReference(this, property))
  }

  def seenGrammars: Set[BiGrammar] = ancestorGrammars + value
  def ancestorGrammars: Set[BiGrammar]
  def ancestors: Seq[GrammarPath]
  def findGrammar(grammar: BiGrammar): Option[GrammarReference] = find(p => p.value == grammar)

  def findAsNode(shape: NodeShape): GrammarReference = {
    find(p => p.value match { case node: NodeGrammar => node.shape == shape; case _ => false}).get
  }

  def findAs(field: NodeField): GrammarReference = {
    find(p => p.value match { case as:As => as.field == field; case _ => false}).get
  }

  def findLabelled(label: Key): GrammarReference = {
    find(p => p.value match { case as:Labelled => as.name == label; case _ => false}).get
  }

  def find(predicate: GrammarPath => Boolean): Option[GrammarReference] = {
    var result: Option[GrammarReference] = None
    GraphBasics.traverseBreadth[GrammarPath](Seq(this),
      path => path.newChildren,
      path =>
        if (predicate(path)) {
          result = Some(path).collect({case x: GrammarReference => x})
          Halt
        }
        else {
          Continue
        }
    )
    result
  }

  def descendants: Seq[GrammarReference] = selfAndDescendants.drop(1).collect { case x:GrammarReference => x }
  def selfAndDescendants: Seq[GrammarPath] = GraphBasics.traverseBreadth[GrammarPath](Seq(this),
    path => path.newChildren)
}
