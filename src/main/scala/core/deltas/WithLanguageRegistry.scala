package core.deltas

import core.deltas.node.{Key, NodeClass}

import scala.collection.mutable

trait WithLanguageRegistry extends Key {
  type Registry

  type ClassRegistry[Registration] = mutable.HashMap[NodeClass, Registration]
  def createRegistry: Registry
  def getRegistry(language: Language): Registry = language.data.getOrElseUpdate(this, createRegistry).asInstanceOf[Registry]
}

trait WithCompilationState {
  type State

  def createState: State
  def getState(compilation: Compilation): State = compilation.state.getOrElseUpdate(this, createState).asInstanceOf[State]
}