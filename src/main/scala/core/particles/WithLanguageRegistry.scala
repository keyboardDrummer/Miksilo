package core.particles

import scala.collection.mutable

trait WithLanguageRegistry {
  type Registry

  type ClassRegistry[Registration] = mutable.HashMap[Any, Registration]
  def createRegistry: Registry
  def getRegistry(state: Language): Registry = state.data.getOrElseUpdate(this, createRegistry).asInstanceOf[Registry]
}

trait WithCompilationState {
  type State

  def createState: State
  def getState(state: Compilation): State = state.state.getOrElseUpdate(this, createState).asInstanceOf[State]
}