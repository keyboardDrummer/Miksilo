package core.deltas

import core.language.node.{Key, NodeShape}
import core.language.Language

import scala.collection.mutable

@deprecated("Use ShapeAspect instead", "")
trait WithLanguageRegistry extends Key {
  type Registry

  type ShapeRegistry[Registration] = mutable.HashMap[NodeShape, Registration]
  def createRegistry: Registry
  def getRegistry(language: Language): Registry = language.data.getOrElseUpdate(this, createRegistry).asInstanceOf[Registry]
}

