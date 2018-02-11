package core.deltas

import core.deltas.node.NodeShape
import core.language.Language

import scala.collection.mutable

/*
Decorates each Shape in a language with a property of type T.
 */
class ShapeProperty[T] {

  private def map(language: Language): mutable.Map[NodeShape, T] =
    language.data.getOrElseUpdate(this, mutable.Map.empty[NodeShape, T]).asInstanceOf[mutable.Map[NodeShape, T]]

  def get(language: Language): scala.collection.Map[NodeShape, T] = map(language)

  def get(language: Language, shape: NodeShape): T = {
    map(language)(shape)
  }

  def add[U <: T with HasShape](language: Language, value: U): Unit = add(language, value.shape, value)

  def add(language: Language, shape: NodeShape, value: T): Unit = map(language).put(shape, value)
}
