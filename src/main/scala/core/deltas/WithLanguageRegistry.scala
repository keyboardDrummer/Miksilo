package core.deltas

import core.deltas.node.{Key, NodeShape}
import core.language.Language

import scala.collection.mutable

trait HasShape {
  def shape: NodeShape
}

class ShapeAspect[T] {

  private def map(language: Language): mutable.Map[NodeShape, T] =
    language.data.getOrElseUpdate(this, mutable.Map.empty[NodeShape, T]).asInstanceOf[mutable.Map[NodeShape, T]]

  def get(language: Language, shape: NodeShape): T = {
    map(language)(shape)
  }

  def add[U <: T with HasShape](language: Language, value: U): Unit = add(language, value.shape, value)

  def add(language: Language, shape: NodeShape, value: T): Unit = map(language).put(shape, value)
}

trait WithLanguageRegistry extends Key {
  type Registry

  type ShapeRegistry[Registration] = mutable.HashMap[NodeShape, Registration]
  def createRegistry: Registry
  def getRegistry(language: Language): Registry = language.data.getOrElseUpdate(this, createRegistry).asInstanceOf[Registry]
}

trait WithCompilationState {
  type State

  def createState: State
  def getState(compilation: Compilation): State = compilation.state.getOrElseUpdate(this, createState).asInstanceOf[State]
}