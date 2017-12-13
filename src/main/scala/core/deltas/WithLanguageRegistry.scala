package core.deltas

import core.deltas.node.{Key, NodeShape}

import scala.collection.mutable

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