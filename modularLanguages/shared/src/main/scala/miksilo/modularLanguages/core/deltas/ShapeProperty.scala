package miksilo.modularLanguages.core.deltas

import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.node.NodeShape

import scala.collection.mutable

/*
Decorates each Shape in a language with a property of type T.
 */
class ShapeProperty[T] {

  private def map(language: Language): mutable.HashMap[NodeShape, T] =
    language.data.getOrElseUpdate(this, new mutable.HashMap[NodeShape, T]).asInstanceOf[mutable.HashMap[NodeShape, T]]

  def get(language: Language): scala.collection.Map[NodeShape, T] = map(language)

  def apply(language: Language, shape: NodeShape) : T = get(language, shape).get

  def get(language: Language, shape: NodeShape): Option[T] = {
    map(language).get(shape)
  }

  def add[U <: T with HasShape](language: Language, value: U): Unit = add(language, value.shape, value)

  def add(language: Language, shape: NodeShape, value: T): Unit = map(language).put(shape, value)

  def change(language: Language, shape: NodeShape, updater: T => T): Unit = {
    val original = this(language, shape)
    add(language, shape, updater(original))
  }
}

class Property[T](default: => T = null) {

  private def map(language: Language): T =
    language.data.getOrElseUpdate(this, default).asInstanceOf[T]

  def get(language: Language): T = map(language)

  def add(language: Language, value: T): Unit = language.data.put(this, value)
}